// From https://llvm.org/bugs/show_bug.cgi?id=3120
#include <stdlib.h>
#include <stdbool.h>

//////////// From intepret.h
#if !defined(__NO_INLINE__) && defined(__linux__)
#define __NO_INLINE__
#endif
#include <inttypes.h>
typedef uint8_t Opcode;
typedef const Opcode *Opcodes;
typedef int (*Evaluator)(Opcodes);
typedef struct { Evaluator fn; const char *name; } Interpreter;
extern Interpreter interpreters[];
////////////////////////

enum Operand {
  RETURN = 0, INCREMENT = 1, DECREMENT = 2, DOUBLE = 3, SWAPWORD = 4,
};

static int interpret_switch(Opcodes opcodes) {
  int result = 0;
  while (true) {
    switch (*opcodes++) {
      case RETURN: return result;
      case INCREMENT: result++; break;
      case DECREMENT: result--; break;
      case DOUBLE: result <<= 1; break;
      case SWAPWORD: result = (result << 16) | (result >> 16); break;
    }
  }
}

static int interpret_threaded(Opcodes opcodes) {
  static const void *codetable[] =
    { &&RETURN, &&INCREMENT, &&DECREMENT, &&DOUBLE, &&SWAPWORD };
  int result = 0;

  goto *codetable[*(opcodes++)];

 RETURN:
  return result;

 INCREMENT:
  result++;
  goto *codetable[*(opcodes++)];

 DECREMENT:
  result--;
  goto *codetable[*(opcodes++)];

 DOUBLE:
  result <<= 1;
  goto *codetable[*(opcodes++)];

 SWAPWORD:
  result = (result << 16) | (result >> 16);
  goto *codetable[*(opcodes++)];
}

int interpret_threaded_2(Opcodes opcodes) {
  void *codetable[] = { &&RET, &&INC, &&DEC, &&DBL, &&ZERO };
  int result = 0;

  goto *codetable[*(opcodes++)];

 RET:
  return result;

 INC:
  result++;
  goto *codetable[*(opcodes++)];

 DEC:
  result--;
  goto *codetable[*(opcodes++)];

 DBL:
  result <<= 1;
  goto *codetable[*(opcodes++)];

 ZERO:
  result = (result << 16) | (result >> 16);
  goto *codetable[*(opcodes++)];
}

// ---------------------------------------------------------------------------
//
// ---------------------------------------------------------------------------

static struct {
  Opcodes opcodes;
  int result;
} state;

typedef int (Visitor)();

#define VISIT() codetable[state.opcodes++[0]]()

static Visitor returnval, increment, decrement, doubleval, swapword;
static Visitor *codetable[] = {
  [RETURN] = returnval,
  [INCREMENT] = increment,
  [DECREMENT] = decrement,
  [DOUBLE] = doubleval,
  [SWAPWORD] = swapword,
};

static int returnval()
{
  return state.result;
}

static int increment()
{
  state.result += 1;
  return VISIT();
}

static int decrement()
{
  state.result -= 1;
  return VISIT();
}

static int doubleval()
{
  state.result <<= 1;
  return VISIT();
}

static int swapword()
{
  state.result =  (state.result << 16) | (state.result >> 16);
  return VISIT();
}

static int interpret_recursive(Opcodes opcodes) {
  state.opcodes = opcodes;
  state.result = 0;
  return VISIT();
}

#if defined(__BLOCKS__)
//#ifndef __APPLE__
// HACK!
static void _NSConcreteGlobalBlock(void) { }
//#endif

typedef int (^BlockVisitor)(Opcodes opcodes, int r);

#define VISIT_NEXT(o, r) blocktable[*o](o + 1, r)

static const BlockVisitor blocktable[] = {
  [RETURN] = ^(Opcodes opcodes, int r) {
    (void)opcodes;
    return r;
  },

  [INCREMENT] = ^(Opcodes opcodes, int r) {
    return VISIT_NEXT(opcodes, r + 1);
  },

  [DECREMENT] = ^(Opcodes opcodes, int r) {
    return VISIT_NEXT(opcodes, r - 1);
  },

  [DOUBLE] = ^(Opcodes opcodes, int r) {
    return VISIT_NEXT(opcodes, r << 1);
  },

  [SWAPWORD] = ^(Opcodes opcodes, int r) {
    return VISIT_NEXT(opcodes, (r << 16) | (r >> 16));
  },
};

static int interpret_blocks(Opcodes opcodes) {
  return VISIT_NEXT(opcodes, 0);
}
#endif

Interpreter interpreters[] =
  {
    { &interpret_switch, "Switched interpreter" },
    { &interpret_threaded, "Threaded interpreter" },
    { &interpret_recursive, "Recursive interpreter" },
#ifdef __BLOCKS__
    { &interpret_blocks, "Closure-based interpreter" },
#endif
    { NULL, NULL },
  };
