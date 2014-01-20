#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "erl_interface.h"
#include "ei.h"

#define BUFSIZE 1000

int my_listen(int);
int foo(int);
int bar(int);

int main(int argc, char **argv) {
  struct sockaddr_in addr;                 /* our address */
  int on = 1;
  int port;                                /* Listen port number */
  int listen_fd;                           /* Listen socket */
  int fd;                                  /* fd to Erlang node */
  ErlConnect conn;                         /* Connection data */

  int got;                                 /* Result of receive */
  unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
  ErlMessage emsg;                         /* Incoming message */

  ETERM *fromp, *tuplep, *fnp, *argp, *resp;
  int res;

  if (argc < 2) {
    fprintf(stderr,"Usage: %s portnumber\n",argv[0]);
    return 1;
  }

  port = atoi(argv[1]);

  erl_init(NULL, 0);

  if (erl_connect_init(3, "secretcookie", 0) < 0) {
    erl_err_quit("erl_connect_init");
  }

  if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    perror("socket");
    return 0;
  }

  if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)) < 0) {
    perror("setsockopt");
    close(listen_fd);
    return 0;
  }

  memset((void*) &addr, 0, (size_t) sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0) {
    perror("bind");
    close(listen_fd);
    return 0;
  }

  if (listen(listen_fd, 5) < 0) {
    perror("listen");
    close(listen_fd);
    return 0;
  }

  if (erl_publish(port) == -1) {
    close(listen_fd);
    erl_err_quit("erl_publish");
  }
  fprintf(stderr, "published c3\n\r");

  if ((fd = erl_accept(listen_fd, &conn)) == ERL_ERROR) {
    close(listen_fd);
    erl_err_quit("erl_accept");
  }
  fprintf(stderr, "Connected to %s\n\r", conn.nodename);

  while (1) {
    got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
    if (got == ERL_TICK) {
      /* ignore */
    } else if (got == ERL_ERROR) {
      close(fd);
      close(listen_fd);
      return 0;
    } else {
      if (emsg.type == ERL_REG_SEND) {
        fromp = erl_element(2, emsg.msg);
        tuplep = erl_element(3, emsg.msg);
        fnp = erl_element(1, tuplep);
        argp = erl_element(2, tuplep);

        if (strncmp(ERL_ATOM_PTR(fnp), "foo", 3) == 0) {
          res = foo(ERL_INT_VALUE(argp));
        } else if (strncmp(ERL_ATOM_PTR(fnp), "bar", 3) == 0) {
          res = bar(ERL_INT_VALUE(argp));
        }

        resp = erl_format("{cnode, ~i}", res);
        erl_send(fd, fromp, resp);

        erl_free_term(emsg.from); erl_free_term(emsg.msg);
        erl_free_term(fromp); erl_free_term(tuplep);
        erl_free_term(fnp); erl_free_term(argp);
        erl_free_term(resp);
      }
    }
  }
}

int foo(int i) {
  return i++;
}

int bar(int i) {
  return i*2;
}
