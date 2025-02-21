#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include "sdlinterf.h"

typedef struct Mark {
	double x;
	double y;
	double dir;
	struct Mark *prev;
} Mark;

Mark *top_mark;

double __ttl_x = 0.0, __ttl_y = 0.0;
double __ttl_dir = 0.0, __ttl_delay = 1.0;
double __ttl_args[10] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
double __ttl_max_x = 20.0, __ttl_max_y = 15.0;
double __ttl_red = 100.0, __ttl_green = 100.0, __ttl_blue = 0.0;

double __ttl_dist() {
	return sqrt(__ttl_x * __ttl_x + __ttl_y * __ttl_y);
}

void __ttl_set_col(double r, double g, double b) {
	__ttl_red = r;
	__ttl_green = g;
	__ttl_blue = b;
}

void __ttl_init(int argc, const char *argv[]) {
	srand(time(NULL));
	sdlInit();
	for (int i = 1; i < argc && i < 10; ++i) {
		__ttl_args[i] = atof(argv[i]);
	}
}

void __ttl_stop() {
	printf("halt and catch fire\n");
	while (1) {
		sdlMilliSleep(200);
	}
}

static int to_color(double x) {
  if (x <= 0) return 0;
  if (x >= 100) return 255;
  return lround(x / 100 * 255);
}

void __ttl_walk_pos(double next_x, double next_y, bool draw) {
	if (draw) {
		int from_x = (int) (SDL_X_SIZE / 2.0 * (1.0 + __ttl_x / __ttl_max_x));
		int from_y = (int) (SDL_Y_SIZE / 2.0 * (1.0 + __ttl_y / __ttl_max_y));
		int to_x = (int) (SDL_X_SIZE / 2.0 * (1.0 + next_x / __ttl_max_x));
		int to_y = (int) (SDL_Y_SIZE / 2.0 * (1.0 + next_y / __ttl_max_y));
		if (from_x < SDL_X_SIZE && from_x >= 0
			&& from_y < SDL_Y_SIZE && from_y >= 0
			&& to_x < SDL_X_SIZE && to_x >= 0
			&& to_y < SDL_Y_SIZE && to_y >= 0) {
			sdlDrawLine(
				from_x, from_y, to_x, to_y,
				to_color(__ttl_red),
				to_color(__ttl_green),
				to_color(__ttl_blue)
			);
			sdlUpdate();
			sdlMilliSleep((int) __ttl_delay);
		}
	}
	__ttl_x = next_x;
	__ttl_y = next_y;
}

void __ttl_walk(double dist, bool draw) {
	double next_x = __ttl_x + dist * cos(__ttl_dir * M_PI / 180.0);
	double next_y = __ttl_y - dist * sin(__ttl_dir * M_PI / 180.0);
	__ttl_walk_pos(next_x, next_y, draw);
}

void __ttl_set_mark() {
	Mark *m = malloc(sizeof(Mark));
	m->x = __ttl_x;
	m->y = __ttl_y;
	m->dir = __ttl_dir;
	m->prev = top_mark;
	top_mark = m;
}

void __ttl_load_mark(bool draw) {
	if (top_mark == NULL) {
		fprintf(stderr, "no mark");
		exit(1);
	}
	Mark *m = top_mark;
	top_mark = m->prev;
	__ttl_dir = m->dir;
	__ttl_walk_pos(m->x, m->y, draw);
	free(m);
}

double __ttl_rand(double min, double max) {
	return min + ((rand() % 32768) * (max - min) / 32768.0);
}

void __ttl_set_dir(double new_dir) {
	__ttl_dir = fmod(fmod(new_dir, 360.0) + 360.0, 360.0);
}