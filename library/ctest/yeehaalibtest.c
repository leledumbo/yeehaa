#include <stdio.h>
#include "yeehaalib.h"

#ifdef __linux__
#include <unistd.h>
#endif

#ifdef _WIN32
#include <windows.h>
#endif

int main() {
  yeeconn_t* yc = InitializeConnection(9999, 100);

  puts("Waiting for the bulbs to be discovered...");

  // wait for 2 seconds, hoping all the bulbs have been registered
#ifdef __linux__
  usleep(2000000);
#endif
#ifdef _WIN32
  Sleep(2000);
#endif

  bulbinfolist_t *bil = GetBulbInfos(yc);
  for (int i = 0; i < bil->count; i++) {
    bulbinfo_t *bi = &bil->data[i];
    printf("%d: ip=%s,model=%s,power=%s,brightness=%s,rgb=%s\n", i + 1, bi->ip, bi->model, bi->power, bi->brightness, bi->rgb);
  }

  FreeBulbInfos(bil);
  FinalizeConnection(yc);
  return 0;
}