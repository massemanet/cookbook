#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <sys/utsname.h>

int
main(int argc, char **argv)
{
  void *handle;
  double (*cosine)(double);
  char *error;
  struct utsname unam;

  uname(&unam);
  if (strcmp(unam.sysname,"Darwin") == 0) {
    handle = dlopen("libm.dylib", RTLD_LAZY);
  }else{
    handle = dlopen("libm.so", RTLD_LAZY);
  }
  if (!handle) {
    fprintf(stderr, "%s\n", dlerror());
    exit(EXIT_FAILURE);
  }

  dlerror();    /* Clear any existing error */

  *(void **) (&cosine) = dlsym(handle, "cos");

  if ((error = dlerror()) != NULL)  {
    fprintf(stderr, "%s\n", error);
    exit(EXIT_FAILURE);
  }

  printf("%f\n", (*cosine)(2.0));
  dlclose(handle);
  exit(EXIT_SUCCESS);
}
