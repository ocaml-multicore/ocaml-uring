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

#define Ring_val(v) (struct io_uring*)Data_custom_val(v);

#define EVENT_TYPE_READ 0
#define EVENT_TYPE_WRITE 1
#define EVENT_TYPE_CLOSE 2

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
    value callback;
    value buffer;
};

value ring_setup(value entries) {
    CAMLparam1(entries);

    struct io_uring* ring = (struct io_uring*)malloc(sizeof(struct io_uring));

    int status = io_uring_queue_init(Long_val(entries), ring, 0);

    if( status ) {
        // Everything was set up fine
        value ring_custom = caml_alloc_custom_mem(&ring_ops, sizeof(struct io_uring*), sizeof(struct io_uring));

        CAMLreturn(ring_custom);
    } else {
        // Something did not go well, raise an exception
        char* error_msg = strerror(status);
        caml_failwith(error_msg);
    }
}

void ring_queue_write(value ring_custom, int fd, value callback, value buffer_bigarray, value nbytes, value offset) {
    CAMLparam5(ring_custom, callback, buffer_bigarray, nbytes, offset);

    struct io_uring* ring = Ring_val(ring_custom);
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);

    char* buf = (char*)Caml_ba_data_val(buffer_bigarray);

    io_uring_prep_write(sqe, fd, buf, Long_val(nbytes), Long_val(offset));

    struct request* req = (struct request*)caml_stat_alloc(sizeof(struct request));

    req->event_type = EVENT_TYPE_WRITE;

    caml_register_generational_global_root(&req->buffer);
    caml_register_generational_global_root(&req->callback);

    io_uring_sqe_set_data(sqe, req);

    CAMLreturn0;
}

void ring_queue_read(value ring_custom, int fd, value callback, value buffer_bigarray, value nbytes, value offset) {
    CAMLparam5(ring_custom, callback, buffer_bigarray, nbytes, offset);

    struct io_uring* ring = Ring_val(ring_custom);
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);

    char* buf = (char*)Caml_ba_data_val(buffer_bigarray);

    io_uring_prep_read(sqe, fd, buf, Long_val(nbytes), Long_val(offset));

    struct request* req = (struct request*)caml_stat_alloc(sizeof(struct request));

    req->event_type = EVENT_TYPE_READ;

    caml_register_generational_global_root(&req->buffer);
    caml_register_generational_global_root(&req->callback);

    io_uring_sqe_set_data(sqe, req);

    CAMLreturn0;
}


void ring_queue_close(value ring_custom, int fd) {
    CAMLparam1(ring_custom);

    struct io_uring* ring = Ring_val(ring_custom);
    struct io_uring_sqe *sqe = io_uring_get_sqe(ring);

    io_uring_prep_close(sqe, fd);

    struct request* req = (struct request*)caml_stat_alloc(sizeof(struct request));

    req->event_type = EVENT_TYPE_CLOSE;

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

    if( ret < 0 ) {
        caml_failwith(strerror(ret));
    }

    struct request* req = io_uring_cqe_get_data(cqe);

    if( cqe->res < 0 ) {
        caml_failwith(strerror(cqe->res));
    }

    if( req->event_type == EVENT_TYPE_READ || req->event_type == EVENT_TYPE_WRITE ) {
        if( req->event_type == EVENT_TYPE_READ ) {
            caml_callback2(req->callback, req->buffer, Val_long(cqe->res));
        }

        caml_remove_generational_global_root(&req->caml_callback);
        caml_remove_generational_global_root(&req->buffer);
    }

    caml_stat_free(req);

    CAMLreturn0;
}

value ring_submit(value ring_custom) {
    CAMLparam1(ring_custom);

    struct io_uring* ring = Ring_val(ring_custom);

    int submitted = io_uring_submit(ring);

    CAMLreturn(submitted);
}

void ring_exit(value ring_custom) {
    CAMLparam1(ring_custom);

    struct io_uring* ring = Ring_val(ring_custom);

    io_uring_queue_exit(ring);

    free(ring);

    CAMLreturn0;
}