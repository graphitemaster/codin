#include <stdio.h>

#include "utility.h"
#include "strbuf.h"

static Bool sizefile(FILE *fp, Uint64 *size) {
	if (fseek(fp, 0, SEEK_END) != 0) {
		return false;
	}

	const long tell = ftell(fp);
	if (tell < 0) {
		fseek(fp, 0, SEEK_SET);
		return false;
	}

	fseek(fp, 0, SEEK_SET);
	*size = CAST(Uint64, tell);

	return true;
}

Array(Uint8) _readfile(String filename, Context *context) {
	// Need to introduce the NUL to call fopen.
	StrBuf strbuf;
	strbuf_init(&strbuf, context);
	strbuf_put_string(&strbuf, string_unquote(filename, "\""));
	strbuf_put_rune(&strbuf, '\0');
	FILE *fp = fopen(RCAST(const char *, strbuf.contents), "rb");
	if (!fp) {
		return 0;
	}

	Array(Uint8) result = 0;
	Uint64 size = 0;
	if (!sizefile(fp, &size)) {
		while (!feof(fp)) {
			const Uint8 ch = fgetc(fp);
			if (!array_push(result, ch)) {
				array_free(result);
				return 0;
			}
		}
		return result;
	}

	if (!array_resize(result, size)) {
		goto L_error;
	}

	if (fread(result, size, 1, fp) != 1) {
		goto L_error;
	}

	fclose(fp);
	return result;

L_error:
	fclose(fp);
	array_free(result);
	return 0;
}

// Half float support
static const Uint32 CODIN_f16_base[] = {
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000001, 0x00000002,
	0x00000004, 0x00000008, 0x00000010, 0x00000020, 0x00000040,
	0x00000080, 0x00000100, 0x00000200, 0x00000400, 0x00000800,
	0x00000c00, 0x00001000, 0x00001400, 0x00001800, 0x00001c00,
	0x00002000, 0x00002400, 0x00002800, 0x00002c00, 0x00003000,
	0x00003400, 0x00003800, 0x00003c00, 0x00004000, 0x00004400,
	0x00004800, 0x00004c00, 0x00005000, 0x00005400, 0x00005800,
	0x00005c00, 0x00006000, 0x00006400, 0x00006800, 0x00006c00,
	0x00007000, 0x00007400, 0x00007800, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00, 0x00007c00,
	0x00007c00, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008000,
	0x00008000, 0x00008000, 0x00008000, 0x00008000, 0x00008001,
	0x00008002, 0x00008004, 0x00008008, 0x00008010, 0x00008020,
	0x00008040, 0x00008080, 0x00008100, 0x00008200, 0x00008400,
	0x00008800, 0x00008c00, 0x00009000, 0x00009400, 0x00009800,
	0x00009c00, 0x0000a000, 0x0000a400, 0x0000a800, 0x0000ac00,
	0x0000b000, 0x0000b400, 0x0000b800, 0x0000bc00, 0x0000c000,
	0x0000c400, 0x0000c800, 0x0000cc00, 0x0000d000, 0x0000d400,
	0x0000d800, 0x0000dc00, 0x0000e000, 0x0000e400, 0x0000e800,
	0x0000ec00, 0x0000f000, 0x0000f400, 0x0000f800, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00, 0x0000fc00,
	0x0000fc00, 0x0000fc00
};

static const Uint8 CODIN_f16_shift[] = {
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x17,
	0x16, 0x15, 0x14, 0x13, 0x12, 0x11, 0x10, 0x0f, 0x0e, 0x0d, 0x0d, 0x0d, 0x0d,
	0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d,
	0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x0d, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x17, 0x16, 0x15, 0x14, 0x13,
	0x12, 0x11, 0x10, 0x0f, 0x0e, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d,
	0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d,
	0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x0d, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	0x18, 0x18, 0x18, 0x18, 0x0d
};

Float16 f32_to_f16(Float32 f) {
	const union { Float32 f; Uint32 u; } s = { f };
	const Uint32 e = s.u >> 23;
	const Uint32 base = CODIN_f16_base[e & 0x1ff];
	const Uint8 shift = CODIN_f16_shift[e & 0x1ff];
	return base + ((s.u & 0x007fffff) >> shift);
}

Float32 f16_to_f32(Float16 f) {
	union { Uint32 u; Float32 f; } o = { CAST(Uint32, (f & 0x7fff) << 13) };
	const Uint32 exp = 0x0f800000 & o.u;
	o.u += 0x38000000;
	if (exp == 0x0f800000) o.u += 0x38000000;
	if (exp == 0x00000000) o.u += 0x00000001, o.f -= 0x1p-14;
	o.u |= (f & 0x8000) << 16;
	return o.f;
}