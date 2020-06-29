#ifndef YEEHAALIB_H
#define YEEHAALIB_H

#include <stdint.h>

typedef void* yeeconn_t;

typedef struct {
  char *ip,*model,*power,*brightness,*rgb;
} bulbinfo_t;

typedef struct {
  int64_t count;
  bulbinfo_t* data;
} bulbinfolist_t;

yeeconn_t InitializeConnection(uint16_t listenPort,int32_t broadcastIntervalMillisecond);
void FinalizeConnection(yeeconn_t yc);
bulbinfolist_t* GetBulbInfos(yeeconn_t yc);
void FreeBulbInfos(bulbinfolist_t* bil);

#endif // YEEHAALIB_H
