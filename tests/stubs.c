#include <caml/mlvalues.h>
#include <signal.h>

static void handle_signal(int signum) {}

CAMLprim value test_set_signal(value v_unit) {
  struct sigaction sa;

  sa.sa_handler = handle_signal;
  sa.sa_flags = 0;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGCHLD, &sa, NULL);
  return Val_unit;
}
