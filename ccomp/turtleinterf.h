#ifndef __TURTLE_H
#define __TURTLE_H

#include <stdbool.h>

extern double __ttl_x, __ttl_y, __ttl_max_x, __ttl_max_y, __ttl_dir, __ttl_delay;
extern double __ttl_red, __ttl_green, __ttl_blue, __ttl_args[10];

extern double __ttl_dist();
extern void __ttl_init(int, const char *[]);
extern void __ttl_stop();
extern void __ttl_walk_pos(double, double, bool);
extern void __ttl_walk(double, bool);
extern void __ttl_set_mark();
extern void __ttl_load_mark(bool);
extern double __ttl_rand(double, double);
extern void __ttl_set_dir(double); 

#endif