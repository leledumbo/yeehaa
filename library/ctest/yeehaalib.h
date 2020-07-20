#ifndef YEEHAALIB_H
#define YEEHAALIB_H

#include <stdint.h>
#include <stdbool.h>

// data types

typedef void* yeeconn_t;

typedef struct {
  char     *ip;
  char     *model;
  bool     powered_on;
  uint8_t  brightness_percentage;
  uint32_t rgb;
} bulb_data_t;

typedef enum { VALUE_TYPE_INTEGER, VALUE_TYPE_STRING } value_type_t;

typedef struct {
  char *key;
  value_type_t value_type;
  union {
    int32_t int_value;
    char    *str_value;
  };
} value_t;

typedef struct {
  bool    is_error;
  int32_t value_count;
  value_t *values;
} command_result_t;

typedef enum { TRANSITION_EFFECT_SMOOTH, TRANSITION_EFFECT_SUDDEN } transition_effect_t;

typedef uint16_t transition_duration_t;

// callbacks

typedef void (*connection_error_callback_t)(char *msg);
typedef void (*bulb_found_callback_t)(bulb_data_t *bulb);
typedef void (*command_result_callback_t)(int32_t id,command_result_t *result);

// APIs

yeeconn_t InitializeConnection(uint16_t listen_port,int32_t broadcast_interval_millisecond);
void FinalizeConnection(yeeconn_t ABroker);
void RegisterOnConnectionErrorCallback(yeeconn_t ABroker, connection_error_callback_t callback);
void RegisterOnBulbFoundCallback(yeeconn_t ABroker, bulb_found_callback_t callback);
void RegisterOnCommandResultCallback(yeeconn_t ABroker, command_result_callback_t callback);
void SetPower(yeeconn_t ABroker, char *ip, bool is_on, transition_effect_t transition_effect, transition_duration_t transition_duration);

#endif // YEEHAALIB_H
