#include <string.h> // memcpy, strlen
#include <stdlib.h> // malloc, free

#include "support.h"

Bool string_compare(const String *lhs, const String *rhs) {
	return lhs->size == rhs->size && memcmp(lhs->data, rhs->data, lhs->size) == 0;
}

Bool string_assign(String *string, const char *source) {
	const Uint64 length = strlen(source);
	void *data = malloc(length);
	if (!data) return false;
	memcpy(data, source, length);
	string->data = CAST(Uint8*, data);
	string->size = length;
	return true;
}

void string_free(String *string) {
	free(string->data);
	string->data = 0;
}