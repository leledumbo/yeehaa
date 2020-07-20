#include <stdio.h>
#include <string.h>
#include "yeehaalib.h"

yeeconn_t* yc;

void print_bulb_found(bulb_data_t *bulb) {
  printf(
    "ip=%s,model=%s,powered_on=%s,brightness=%d,rgb=%d\n",
    bulb->ip,bulb->model,bulb->powered_on ? "true" : "false",bulb->brightness_percentage,bulb->rgb
  );
  // this will toggle your bulb like crazy!
  SetPower(yc, bulb->ip, !bulb->powered_on, TRANSITION_EFFECT_SMOOTH, 1000);
}

void print_command_result(int32_t id,command_result_t *result) {
  char buf[255];

  sprintf(buf,"command_result=%s",result->is_error ? "failed" : "success");
  for (int i = 0; i < result->value_count; i++) {
    value_t *v = &result->values[i];
    int      n = strlen(buf);

    sprintf(&buf[n],",%s=",v->key);

    n = strlen(buf);
    switch (v->value_type) {
      case VALUE_TYPE_INTEGER:
        sprintf(&buf[n],"%d",v->int_value);
        break;
      case VALUE_TYPE_STRING:
        sprintf(&buf[n],"%s",v->str_value);
        break;
      default:
        fprintf(stderr,"It cannot be! %d\n",v->value_type);
    }
  }
  printf("%s\n", buf);
}

int main() {
  yc = InitializeConnection(9999, 1000);

  RegisterOnBulbFoundCallback(yc, print_bulb_found);
  RegisterOnCommandResultCallback(yc, print_command_result);
  getchar();

  FinalizeConnection(yc);
  return 0;
}