/* Constants and enums for testing define-c-constant and define-c-enum */

#define FFI_TEST_MAX_VALUE 65535
#define FFI_TEST_NEG       (-3)
#define FFI_TEST_GREETING  "hello, ffi"
#define FFI_TEST_DOUBLE    2.5
#define FFI_TEST_NEG_DOUBLE (-0.125)
#define FFI_TEST_FLOAT     0.75f
/* A double literal not representable in float; reading it as 'float
   must round it to float precision. */
#define FFI_TEST_TENTH_D   0.1
#define FFI_TEST_TENTH_F   0.1
#define FFI_TEST_INT_AS_DOUBLE 3

enum ffi_test_color {
    FFI_TEST_RED,
    FFI_TEST_GREEN,
    FFI_TEST_BLUE
};

/* Explicitly valued, and wide enough to need more than a signed int. */
enum ffi_test_flags {
    FFI_TEST_F_A = 1,
    FFI_TEST_F_B = 2,
    FFI_TEST_F_WIDE = 0x80000000
};

/* Anonymous enum. */
enum {
    FFI_TEST_ANON_X = 10,
    FFI_TEST_ANON_Y = 20
};

/* Negative enumerators, for a signed base type. */
enum ffi_test_signed {
    FFI_TEST_S_LO = -5,
    FFI_TEST_S_HI = 5
};
