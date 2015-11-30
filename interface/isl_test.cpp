#include "isl/Ctx.hpp"
#include "isl/Set.hpp"
#include "isl/UnionSet.hpp"
#include "isl/ScheduleNode.hpp"
#include "isl/MultiUnionPwAff.hpp"
#include "isl/Val.hpp"

#define ARRAY_SIZE(array) (sizeof(array)/sizeof(*array))

using namespace isl;

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
