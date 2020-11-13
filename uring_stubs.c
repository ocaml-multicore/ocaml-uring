#include <liburing.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <string.h>

#define Ring_val(v) *((struct io_uring**)Data_custom_val(v));

#define EVENT_TYPE_READ 0
#define EVENT_TYPE_WRITE 1
#define EVENT_TYPE_ACCEPT 2
#define EVENT_TYPE_CLOSE 3

static struct custom_operations ring_ops = {
  "uring.ring",
  custom_finalize_default, /* TODO: Finalize should check we've taken down the ring and if not, take it down */
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

struct request {
    int event_type;
    int fd; // used by READ and WRITE
    value callback;
    value buffer; // used by READ and WRITE
    int write_length; // used by WRITE
    int written_length; // used by WRITE
    struct iovec iov; // used by READ and WRITE
    struct sockaddr* sockaddr; // used by ACCEPT
    socklen_t socklen; // used by ACCEPT
};

value ring_setup(value entries) {
    CAMLparam1(entries);

    struct io_uring* ring = (struct io_uring*)malloc(sizeof(struct io_uring));

    int status = io_uring_queue_init(Long_val(entries), ring, 0);

    if( !status ) {
        // Everything was set up fine
        value ring_custom = caml_alloc_custom_mem(&ring_ops, sizeof(struct io_uring*), sizeof(struct io_uring));

        *((struct io_uring**)Data_custom_val(ring_custom)) = ring;

        CAMLreturn(ring_custom);
    } else {
        // Something did not go well, raise an exception
        char* error_msg = strerror(status);
        caml_failwith(error_msg);
    }
}

void ring_queue_write_full(value ring_custom, value fd, value callback, value buffer_bigarray, value nbytes) {
    CAMLparam5(ring_custom, fd, callback, buffer_bigarray, nbytes);

    struct io_uring* ring = Ring_val(ring_custom);
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);

    char* buf = (char*)Caml_ba_data_val(buffer_bigarray);

    struct request* req = (struct request*)caml_stat_alloc(sizeof(struct request));

    req->fd = Int_val(fd);
    req->write_length = Long_val(nbytes);
    req->written_length = 0;
    req->iov.iov_base = buf;
    req->iov.iov_len = req->write_length;

    io_uring_prep_writev(sqe, req->fd, &req->iov, 1, 0);

    req->event_type = EVENT_TYPE_WRITE;
    req->callback = callback;
    req->buffer = buffer_bigarray;

    caml_register_generational_global_root(&req->buffer);
    caml_register_generational_global_root(&req->callback);

    io_uring_sqe_set_data(sqe, req);

    CAMLreturn0;
}

// For now we use readv instead because it's available in 5.1
void ring_queue_read(value ring_custom, value fd, value callback, value buffer_bigarray, value offset) {
    CAMLparam5(ring_custom, fd, callback, buffer_bigarray, offset);

    struct io_uring* ring = Ring_val(ring_custom);
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);

    char* buf = (char*)Caml_ba_data_val(buffer_bigarray);
    size_t buf_len = Caml_ba_array_val(buffer_bigarray)->dim[0];

    printf("buf_len: %ld\n", buf_len);

    struct request* req = (struct request*)caml_stat_alloc(sizeof(struct request));

    req->fd = Int_val(fd);
    req->iov.iov_base = buf;
    req->iov.iov_len = buf_len;

    io_uring_prep_readv(sqe, req->fd, &req->iov, 1, Long_val(offset));

    req->event_type = EVENT_TYPE_READ;
    req->callback = callback;
    req->buffer = buffer_bigarray;

    caml_register_generational_global_root(&req->buffer);
    caml_register_generational_global_root(&req->callback);

    io_uring_sqe_set_data(sqe, req);

    CAMLreturn0;
}


void ring_queue_close(value ring_custom, value fd) {
    CAMLparam2(ring_custom, fd);

    struct io_uring* ring = Ring_val(ring_custom);
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);

    io_uring_prep_close(sqe, Int_val(fd));

    struct request* req = (struct request*)caml_stat_alloc(sizeof(struct request));

    req->event_type = EVENT_TYPE_CLOSE;

    io_uring_sqe_set_data(sqe, req);

    CAMLreturn0;
}

void ring_queue_accept(value ring_custom, value fd, value callback) {
    CAMLparam3(ring_custom, fd, callback);

    struct io_uring* ring = Ring_val(ring_custom);
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);

    struct request* req = (struct request*)caml_stat_alloc(sizeof(struct request));
    req->sockaddr = (struct sockaddr*)caml_stat_alloc(sizeof(struct sockaddr));
    req->socklen = sizeof(struct sockaddr);

    io_uring_prep_accept(sqe, Int_val(fd), req->sockaddr, &req->socklen, 0);

    req->event_type = EVENT_TYPE_ACCEPT;
    req->callback = callback;

    caml_register_generational_global_root(&req->caml_callback);

    io_uring_sqe_set_data(sqe, req);

    CAMLreturn0;
}

void ring_wait(value ring_custom) {
    CAMLparam1(ring_custom);

    struct io_uring* ring = Ring_val(ring_custom);
    struct io_uring_cqe *cqe;

    caml_enter_blocking_section();
    int ret = io_uring_wait_cqe(ring, &cqe);
    caml_leave_blocking_section();

    printf("got event! ret: %d, cqe->res: %d\n", ret, cqe->res);

    if( ret < 0 ) {
        caml_failwith(strerror(-ret));
    }

    struct request* req = io_uring_cqe_get_data(cqe);

    if( cqe->res < 0 ) {
        caml_failwith(strerror(-cqe->res));
    }

    int cleanup_req = 0;

    if( req->event_type == EVENT_TYPE_READ || req->event_type == EVENT_TYPE_WRITE ) {
        switch( req->event_type ) {
            case EVENT_TYPE_READ:
                caml_callback2(req->callback, req->buffer, Val_long(cqe->res));

                cleanup_req = 1;
                break;
            case EVENT_TYPE_WRITE:
                /* check we actually wrote the full length we tried to write */
                if(cqe->res + req->written_length == req->write_length) {
                    /* call the callback if it exists */
                    if( Is_block(req->callback) ) {
                        caml_callback2(req->callback, req->buffer, req->write_length);
                    }

                    cleanup_req = 1;
                } else {
                    // Here we wrote less than the amount we requested

                    // Store how much we wrote
                    req->written_length += cqe->res;

                    // Now we queue up a new write
                    req->iov.iov_base = req->iov.iov_base + cqe->res;
                    req->iov.iov_len = req->write_length - req->written_length;

                    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);

                    io_uring_prep_writev(sqe, req->fd, &req->iov, 1, 0);
                }
        }

        if( cleanup_req ) {
            caml_remove_generational_global_root(&req->callback);
            caml_remove_generational_global_root(&req->buffer);
        }
    }
    else if( req->event_type == EVENT_TYPE_ACCEPT ) {
        caml_callback(req->callback, Val_int(cqe->res));

        free(req->sockaddr);

        caml_remove_generational_global_root(&req->callback);
    }

    io_uring_cqe_seen(ring, cqe);

    if( cleanup_req ) {
        caml_stat_free(req);
    }

    CAMLreturn0;
}

value ring_submit(value ring_custom) {
    CAMLparam1(ring_custom);

    struct io_uring* ring = Ring_val(ring_custom);

    int submitted = io_uring_submit(ring);

    printf("submitted: %d\n", submitted);

    CAMLreturn(Val_int(submitted));
}

void ring_exit(value ring_custom) {
    CAMLparam1(ring_custom);

    struct io_uring* ring = Ring_val(ring_custom);

    io_uring_queue_exit(ring);

    caml_stat_free(ring);

    CAMLreturn0;
}