#include "isl/Ctx.hpp"
#include "isl/Set.hpp"
#include "isl/UnionSet.hpp"
#include "isl/ScheduleNode.hpp"
#include "isl/MultiUnionPwAff.hpp"
#include "isl/Val.hpp"
#include "isl/MultiVal.hpp"
#include "isl/IslException.h"
#include "isl/PwQpolynomial.hpp"
#include "isl/PwAff.hpp"

#define ARRAY_SIZE(array) (sizeof(array)/sizeof(*array))

using namespace isl;

static int test_parse_multi_val(Ctx &C, const char *str) {
  try {
    isl::MultiVal MV = isl::MultiVal::readFromStr(C, str);
  } catch(IslException ex) {
    return -1;
  }
  return 0;
}

static bool test_parse_map_equal(Ctx &C, const char *m1, const char *m2) {
	Map M1 = Map::readFromStr(C, m1);
	Map M2 = Map::readFromStr(C, m2);
	return !M1.isEqual(M2);
}

/* Inputs for isl_pw_qpolynomial_gist tests.
 * "pwqp" is the input, "set" is the context and "gist" is the expected result.
 */
struct {
	const char *pwqp;
	const char *set;
	const char *gist;
} pwqp_gist_tests[] = {
	{ "{ [i] -> i }", "{ [k] : exists a : k = 2a }", "{ [i] -> i }" },
	{ "{ [i] -> i + [ (i + [i/3])/2 ] }", "{ [10] }", "{ [i] -> 16 }" },
	{ "{ [i] -> ([(i)/2]) }", "{ [k] : exists a : k = 2a+1 }",
	  "{ [i] -> -1/2 + 1/2 * i }" },
	{ "{ [i] -> i^2 : i != 0 }", "{ [i] : i != 0 }", "{ [i] -> i^2 }" },
};

static int test_pwqp(Ctx &C) {
  PwQpolynomial PwQp1 = PwQpolynomial::readFromStr(C,
	"{ [i,j,k] -> 1 + 9 * [i/5] + 7 * [j/11] + 4 * [k/13] }");
  PwQpolynomial PwQp2 = PwQpolynomial::readFromStr(C,
	"[j] -> { [i,k] -> 1 + 9 * [i/5] + 7 * [j/11] + 4 * [k/13] }");

  PwQp1.moveDims(DimType::DTParam, 0, DimType::DTIn, 1, 1);
  PwQp1 = PwQp1.sub(PwQp2);
  assert(PwQp1.isZero());

  for (int i = 0; i < ARRAY_SIZE(pwqp_gist_tests); ++i) {
	PwQp1 = PwQpolynomial::readFromStr(C, pwqp_gist_tests[i].pwqp);
	Set S = Set::readFromStr(C, pwqp_gist_tests[i].set);
	PwQp1 = PwQp1.gist(S);
	PwQp2 = PwQpolynomial::readFromStr(C, pwqp_gist_tests[i].gist);
	PwQp1 = PwQp1.sub(PwQp2);
	assert(PwQp1.isZero());
  }

  return 0;
}

int test_parse(Ctx &C) {
  if (test_parse_multi_val(C, "{ A[B[2] -> C[5, 7]] }"))
    return -1;
  if (test_parse_multi_val(C, "[n] -> { [2] }"))
    return -1;
  if (test_parse_multi_val(C, "{ A[4, infty, NaN, -1/2, 2/3] }"))
    return -1;

  { Map M = Map::readFromStr(C, "{ [i] -> [-i] }"); }
  { Map M = Map::readFromStr(C, "{ A[i] -> L[([i/3])] }"); }
  { Map M = Map::readFromStr(C, "{[[s] -> A[i]] -> [[s+1] -> A[i]]}"); }
  { Map M = Map::readFromStr(C, "{ [p1, y1, y2] -> [2, y1, y2] : "
	"p1 = 1 && (y1 <= y2 || y2 = 0) }"); }
  { Map M = Map::readFromStr(C, ""); }

  assert(test_parse_map_equal(C, "{ [x,y]  : [([x/2]+y)/3] >= 1 }",
	"{ [x, y] : 2y >= 6 - x }"));
  assert(test_parse_map_equal(C, "{ [x,y] : x <= min(y, 2*y+3) }",
	"{ [x,y] : x <= y, 2*y + 3 }"));
  assert(test_parse_map_equal(C,
	"{ [x, y] : (y <= x and y >= -3) or (2y <= -3 + x and y <= -4) }",
	"{ [x,y] : x >= min(y, 2*y+3) }"));
  assert(test_parse_map_equal(C,
	"{[new,old] -> [new+1-2*[(new+1)/2],old+1-2*[(old+1)/2]]}",
	"{ [new, old] -> [o0, o1] : "
	"exists (e0 = [(-1 - new + o0)/2], e1 = [(-1 - old + o1)/2]: "
	"2e0 = -1 - new + o0 and 2e1 = -1 - old + o1 and o0 >= 0 and "
	"o0 <= 1 and o1 >= 0 and o1 <= 1) }"));
  assert(test_parse_map_equal(C,
	"{[new,old] -> [new+1-2*[(new+1)/2],old+1-2*[(old+1)/2]]}",
	"{[new,old] -> [(new+1)%2,(old+1)%2]}"));
  assert(test_parse_map_equal(C,
	"[n] -> { [c1] : c1>=0 and c1<=floord(n-4,3) }",
	"[n] -> { [c1] : c1 >= 0 and 3c1 <= -4 + n }"));
  assert(test_parse_map_equal(C,
	"{ [i,j] -> [i] : i < j; [i,j] -> [j] : j <= i }",
	"{ [i,j] -> [min(i,j)] }"));
  assert(test_parse_map_equal(C,
	"{ [i,j] : i != j }",
	"{ [i,j] : i < j or i > j }"));
  assert(test_parse_map_equal(C,
	"{ [i,j] : (i+1)*2 >= j }",
	"{ [i, j] : j <= 2 + 2i }"));
  assert(test_parse_map_equal(C,
	"{ [i] -> [i > 0 ? 4 : 5] }",
	"{ [i] -> [5] : i <= 0; [i] -> [4] : i >= 1 }"));
  assert(test_parse_map_equal(C,
	"[N=2,M] -> { [i=[(M+N)/4]] }",
	"[N, M] -> { [i] : N = 2 and 4i <= 2 + M and 4i >= -1 + M }"));
  assert(test_parse_map_equal(C,
	"{ [x] : x >= 0 }",
	"{ [x] : x-0 >= 0 }"));
  assert(test_parse_map_equal(C,
	"{ [i] : ((i > 10)) }",
	"{ [i] : i >= 11 }"));
  assert(test_parse_map_equal(C,
	"{ [i] -> [0] }",
	"{ [i] -> [0 * i] }"));
  assert(test_parse_map_equal(C,
	"{ [a] -> [b] : (not false) }",
	"{ [a] -> [b] : true }"));
  assert(test_parse_map_equal(C,
	"{ [i] : i/2 <= 5 }",
	"{ [i] : i <= 10 }"));
  assert(test_parse_map_equal(C,
	"{Sym=[n] [i] : i <= n }",
	"[n] -> { [i] : i <= n }"));
  assert(test_parse_map_equal(C,
	"{ [*] }",
	"{ [a] }"));
  assert(test_parse_map_equal(C,
	"{ [i] : 2*floor(i/2) = i }",
	"{ [i] : exists a : i = 2 a }"));
  assert(test_parse_map_equal(C,
	"{ [a] -> [b] : a = 5 implies b = 5 }",
	"{ [a] -> [b] : a != 5 or b = 5 }"));
  assert(test_parse_map_equal(C,
	"{ [a] -> [a - 1 : a > 0] }",
	"{ [a] -> [a - 1] : a > 0 }"));
  assert(test_parse_map_equal(C,
	"{ [a] -> [a - 1 : a > 0; a : a <= 0] }",
	"{ [a] -> [a - 1] : a > 0; [a] -> [a] : a <= 0 }"));
  assert(test_parse_map_equal(C,
	"{ [a] -> [(a) * 2 : a >= 0; 0 : a < 0] }",
	"{ [a] -> [2a] : a >= 0; [a] -> [0] : a < 0 }"));
  assert(test_parse_map_equal(C,
	"{ [a] -> [(a * 2) : a >= 0; 0 : a < 0] }",
	"{ [a] -> [2a] : a >= 0; [a] -> [0] : a < 0 }"));
  assert(test_parse_map_equal(C,
	"{ [a] -> [(a * 2 : a >= 0); 0 : a < 0] }",
	"{ [a] -> [2a] : a >= 0; [a] -> [0] : a < 0 }"));
  assert(test_parse_map_equal(C,
	"{ [a] -> [(a * 2 : a >= 0; 0 : a < 0)] }",
	"{ [a] -> [2a] : a >= 0; [a] -> [0] : a < 0 }"));


  {
	//test_parse_pwqp(ctx, "{ [i] -> i + [ (i + [i/3])/2 ] }");
  }
  {
	Map M = Map::readFromStr(C,
		"{ S1[i] -> [([i/10]),i%10] : 0 <= i <= 45 }"); }
  {

	//test_parse_pwaff(ctx, "{ [i] -> [i + 1] : i > 0; [a] -> [a] : a < 0 }");
  }
  {
//	test_parse_pwqp(ctx, "{ [x] -> ([(x)/2] * [(x)/3]) }");
  }

  return 0;
}

void test_set() {
  Ctx C = Ctx::alloc();
  Set S = Set::readFromStr(C, "{ [i,j] : 0 <= i <= 5 and 0 <= j <= 5 };");
  Set S1 = Set::readFromStr(C, "{ [i,j] : 0 <= i <= 5 and 0 <= j <= 5 };");

  Set R = S.union_(S1);
}

struct {
	int scale_tile;
	int shift_point;
	const char *domain;
	const char *schedule;
	const char *sizes;
	const char *tile;
	const char *point;
} tile_tests[] = {
	{ 0, 0, "[n] -> { S[i,j] : 0 <= i,j < n }",
	  "[{ S[i,j] -> [i] }, { S[i,j] -> [j] }]",
	  "{ [32,32] }",
	  "[{ S[i,j] -> [floor(i/32)] }, { S[i,j] -> [floor(j/32)] }]",
	  "[{ S[i,j] -> [i] }, { S[i,j] -> [j] }]",
	},
	{ 1, 0, "[n] -> { S[i,j] : 0 <= i,j < n }",
	  "[{ S[i,j] -> [i] }, { S[i,j] -> [j] }]",
	  "{ [32,32] }",
	  "[{ S[i,j] -> [32*floor(i/32)] }, { S[i,j] -> [32*floor(j/32)] }]",
	  "[{ S[i,j] -> [i] }, { S[i,j] -> [j] }]",
	},
	{ 0, 1, "[n] -> { S[i,j] : 0 <= i,j < n }",
	  "[{ S[i,j] -> [i] }, { S[i,j] -> [j] }]",
	  "{ [32,32] }",
	  "[{ S[i,j] -> [floor(i/32)] }, { S[i,j] -> [floor(j/32)] }]",
	  "[{ S[i,j] -> [i%32] }, { S[i,j] -> [j%32] }]",
	},
	{ 1, 1, "[n] -> { S[i,j] : 0 <= i,j < n }",
	  "[{ S[i,j] -> [i] }, { S[i,j] -> [j] }]",
	  "{ [32,32] }",
	  "[{ S[i,j] -> [32*floor(i/32)] }, { S[i,j] -> [32*floor(j/32)] }]",
	  "[{ S[i,j] -> [i%32] }, { S[i,j] -> [j%32] }]",
	},
};

int test_tile(Ctx &C) {
  int scale = C.getTileScaleTileLoops();
  int shift = C.getTileShiftPointLoops();

  for (int i = 0; i < ARRAY_SIZE(tile_tests); ++i) {
    C.setTileScaleTileLoops(tile_tests[i].scale_tile);
    C.setTileShiftPointLoops(tile_tests[i].shift_point);

    UnionSet Domain = UnionSet::readFromStr(C, tile_tests[i].domain);
    ScheduleNode Node = ScheduleNode::fromDomain(Domain);
    Node = Node.child(0);
    MultiUnionPwAff Mupa = MultiUnionPwAff::readFromStr(C, tile_tests[i].schedule);
    Node = Node.insertPartialSchedule(Mupa);
    Val Sizes = Val::readFromStr(C, tile_tests[i].sizes);
    Node = Node.bandTile(Sizes);

    Mupa = MultiUnionPwAff::readFromStr(C, tile_tests[i].tile);
    MultiUnionPwAff Mupa2 = Node.bandGetPartialSchedule();
    int equal = Mupa.plainIsEqual(Mupa2);

    Node = Node.child(0);
    Mupa = MultiUnionPwAff::readFromStr(C, tile_tests[i].point);
    Mupa2 = Node.bandGetPartialSchedule();
    if (equal >= 0 && equal)
    	equal = Mupa.plainIsEqual(Mupa2);
    if (equal < 0)
    	return -1;
    if (!equal)
    	isl_die(C.Get(), isl_error_unknown, "unexpected result", return -1);
  }

  C.setTileScaleTileLoops(scale);
  C.setTileShiftPointLoops(shift);

  return 0;
}

struct {
	const char *name;
	int (*fn)(isl::Ctx &C);
} tests [] = {
	{ "tile", &test_tile }
};

int main(int argc, char **argv)
{
	int i;
        isl::Ctx C = isl::Ctx::alloc();

	const char * srcdir = getenv("srcdir");
	assert(srcdir);

	for (i = 0; i < ARRAY_SIZE(tests); ++i) {
		printf("%s\n", tests[i].name);
		if (tests[i].fn(C) < 0)
			return -1;
	}
	return 0;
}
