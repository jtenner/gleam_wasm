import { pascalCase } from "change-case";

// Definitions
const NN = ["32", "64"];
const MM = ["32", "64"];
const SX = ["u", "s"];
const TX = ["f", "i"];
const I32EXTENDSIZE = ["8", "16"];
const I32LOADSIZE = ["8", "16"];
const I64EXTENDSIZE = ["8", "16", "32"];
const IUNOP = ["clz", "ctz", "popcnt"];
const IBINOP = [
	"add",
	"sub",
	"mul",
	"div_{SX}",
	"rem_{SX}",
	"and",
	"or",
	"xor",
	"shl",
	"shr_{SX}",
	"rotl",
	"rotr",
];
const FUNOP = ["abs", "neg", "sqrt", "ceil", "floor", "trunc", "nearest"];
const FBINOP = ["add", "sub", "mul", "div", "min", "max", "copysign"];
const ITESTOP = ["eqz"];
const IRELOP = ["eq", "ne", "lt_{SX}", "gt_{SX}", "le_{SX}", "ge_{SX}"];
const FRELOP = ["eq", "ne", "lt", "gt", "le", "ge"];
const ISHAPE32 = ["i8x16", "i16x8", "i32x4"];
const ISHAPE64 = ["i64x2"];
const ISHAPE = [...ISHAPE32, ...ISHAPE64];
const FSHAPE32 = ["f32x4"];
const FSHAPE64 = ["f64x2"];
const FSHAPE = [...FSHAPE32, ...FSHAPE64];
const SHAPE = [...ISHAPE, ...FSHAPE];
const HALF = ["low", "high"];
// ...
const I64VIRELOP = ["eq", "ne", "lt_s", "gt_s", "le_s", "ge_s"];
const VVUNOP = ["not"];
const VVBINOP = ["and", "andor", "or", "xor"];
const VVTERNOP = ["bitselect"];
const VVTESTOP = ["any_true"];
const VITESTOP = ["all_true"];
const VIRELOP = ["eq", "ne", "lt_{SX}", "gt_{SX}", "le_{SX}", "ge_{SX}"];
const VFRELOP = ["eq", "ne", "lt", "gt", "le", "ge"];
const VIUNOP = ["abs", "neg"];
const VIBINOP = ["add", "sub"];
const VIMINMAXOP = ["min_{SX}", "max_{SX}"];
const VISATBINOP = ["add_sat_{SX}", "sub_sat_{SX}"];
const VISHIFTOP = ["shl", "shr_{SX}"];
const VFUNOP = ["abs", "neg", "sqrt", "ceil", "floor", "trunc", "nearest"];
const VFBINOP = ["add", "sub", "mul", "div", "min", "max", "pmin", "pmax"];
const VUNOP = ["{VIUNOP}", "{VFUNOP}", "popcnt"];
const VBINOP = [
	"{VIBINOP}",
	"{VFBINOP}",
	"{VIMINMAXOP}",
	"{VISATBINOP}",
	"mul",
	"avgr_u",
	"q15mulr_sat_s",
];
const WW = ["8", "16", "32", "64"];

const patterns = {
	NN,
	MM,
	SX,
	TX,
	I32EXTENDSIZE,
	IUNOP,
	IBINOP,
	FUNOP,
	FBINOP,
	ITESTOP,
	IRELOP,
	FRELOP,
	I64EXTENDSIZE,
	ISHAPE,
	FSHAPE,
	ISHAPE32,
	ISHAPE64,
	FSHAPE32,
	FSHAPE64,
	SHAPE,
	HALF,
	I64VIRELOP,
	VVUNOP,
	VVBINOP,
	VVTERNOP,
	VVTESTOP,
	VITESTOP,
	VIRELOP,
	VFRELOP,
	VIUNOP,
	VIBINOP,
	VIMINMAXOP,
	VISATBINOP,
	VISHIFTOP,
	VFUNOP,
	VFBINOP,
	VUNOP,
	VBINOP,
	WW,
	I32LOADSIZE,
};

const instrs = [
	// see: https://webassembly.github.io/gc/core/syntax/instructions.html#numeric-instructions
	"i{NN}.const|(val: I{NN})|ResultType(NoResultTypes, OneResultType(I{NN}ValType))",
	"f{NN}.const|(val: F{NN})|ResultType(NoResultTypes, OneResultType(F{NN}ValType))",
	"i{NN}.{IUNOP}||ResultType(OneResultType(I{NN}ValType), OneResultType(I{NN}ValType))",
	"f{NN}.{FUNOP}||ResultType(OneResultType(F{NN}ValType), OneResultType(F{NN}ValType))",
	"i{NN}.{IBINOP}||ResultType(TwoResultTypes(I{NN}ValType, I{NN}ValType), OneResultType(I{NN}ValType))",
	"f{NN}.{FBINOP}||ResultType(TwoResultTypes(F{NN}ValType, F{NN}ValType), OneResultType(F{NN}ValType))",
	"i{NN}.{ITESTOP}||ResultType(OneResultType(I{NN}ValType), OneResultType(I32ValType))",
	"i{NN}.{IRELOP}||ResultType(TwoResultTypes(I{NN}ValType, I{NN}ValType), OneResultType(I32ValType))",
	"f{NN}.{FRELOP}||ResultType(TwoResultTypes(F{NN}ValType, F{NN}ValType), OneResultType(I32ValType))",
	"i32.extend{I32EXTENDSIZE}_s||ResultType(OneResultType(I32ValType), OneResultType(I32ValType))",
	"i64.extend{I64EXTENDSIZE}_s||ResultType(OneResultType(I64ValType), OneResultType(I64ValType))",
	"i32.wrap_i64||ResultType(OneResultType(I64ValType), OneResultType(I32ValType))",
	"i64.extend_i32_{SX}||ResultType(OneResultType(I32ValType), OneResultType(I64ValType))",
	"i{NN}.trunc_f{MM}_{SX}||ResultType(OneResultType(F{MM}ValType), OneResultType(I{NN}ValType))",
	"i{NN}.trunc_sat_f{MM}_{SX}||ResultType(OneResultType(F{MM}ValType), OneResultType(I{NN}ValType))",
	"f32.demote_f64||ResultType(OneResultType(F32ValType), OneResultType(F64ValType))",
	"f64.promote_f32||ResultType(OneResultType(F64ValType), OneResultType(F32ValType))",
	"f{NN}.convert_i{MM}_{SX}||ResultType(OneResultType(I{MM}ValType), OneResultType(F{NN}ValType))",
	"f{NN}.reinterpret_i{MM}||ResultType(OneResultType(I{MM}ValType), OneResultType(F{MM}ValType))",
	"i{NN}.reinterpret_f{MM}||ResultType(OneResultType(F{MM}ValType), OneResultType(I{MM}ValType))",

	// https://webassembly.github.io/gc/core/syntax/instructions.html#vector-instructions
	"v128.const|(val: V128Value)|ResultType(NoResultTypes, OneResultType(V128ValType))",
	"v128.{VVUNOP}||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",
	"v128.{VVBINOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"v128.{VVTERNOP}||ResultType(ThreeResultTypes(V128ValType, V128ValType, V128ValType), OneResultType(V128ValType))",
	"v128.{VVTESTOP}||ResultType(OneResultType(V128ValType), OneResultType(I32ValType))",
	"i8x16.shuffle|(idx: LaneIDX16)|ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i8x16.swizzle||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"{ISHAPE32}.splat||ResultType(OneResultType(I32ValType), OneResultType(V128ValType))",
	"{ISHAPE64}.splat||ResultType(OneResultType(I64ValType), OneResultType(V128ValType))",
	"{FSHAPE32}.splat||ResultType(OneResultType(F32ValType), OneResultType(V128ValType))",
	"{FSHAPE64}.splat||ResultType(OneResultType(F64ValType), OneResultType(V128ValType))",
	"i8x16.extract_lane_s|(idx: LaneIDX16)|ResultType(OneResultType(V128ValType), OneResultType(I32ValType))",
	"i16x8.extract_lane_s|(idx: LaneIDX8)|ResultType(OneResultType(V128ValType), OneResultType(I32ValType))",
	"i32x4.extract_lane_s|(idx: LaneIDX4)|ResultType(OneResultType(V128ValType), OneResultType(I32ValType))",
	"i64x2.extract_lane_s|(idx: LaneIDX2)|ResultType(OneResultType(V128ValType), OneResultType(I64ValType))",
	"i8x16.extract_lane|(idx: LaneIDX16)|ResultType(OneResultType(V128ValType), OneResultType(I32ValType))",
	"i16x8.extract_lane|(idx: LaneIDX8)|ResultType(OneResultType(V128ValType), OneResultType(I32ValType))",
	"i32x4.extract_lane|(idx: LaneIDX4)|ResultType(OneResultType(V128ValType), OneResultType(I32ValType))",
	"i64x2.extract_lane|(idx: LaneIDX2)|ResultType(OneResultType(V128ValType), OneResultType(I64ValType))",
	"f32x4.extract_lane|(idx: LaneIDX4)|ResultType(OneResultType(V128ValType), OneResultType(F32ValType))",
	"f64x2.extract_lane|(idx: LaneIDX2)|ResultType(OneResultType(V128ValType), OneResultType(F64ValType))",
	"i8x16.replace_lane|(idx: LaneIDX16)|ResultType(TwoResultTypes(V128ValType, I32ValType), OneResultType(V128ValType))",
	"i16x8.replace_lane|(idx: LaneIDX8)|ResultType(TwoResultTypes(V128ValType, I32ValType), OneResultType(V128ValType))",
	"i32x4.replace_lane|(idx: LaneIDX4)|ResultType(TwoResultTypes(V128ValType, I32ValType), OneResultType(V128ValType))",
	"i64x2.replace_lane|(idx: LaneIDX2)|ResultType(TwoResultTypes(V128ValType, I64ValType), OneResultType(V128ValType))",
	"f32x4.replace_lane|(idx: LaneIDX4)|ResultType(TwoResultTypes(V128ValType, F32ValType), OneResultType(V128ValType))",
	"f64x2.replace_lane|(idx: LaneIDX2)|ResultType(TwoResultTypes(V128ValType, F64ValType), OneResultType(V128ValType))",
	"i8x16.{VIRELOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i16x8.{VIRELOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i32x4.{VIRELOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i64x2.{I64VIRELOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"{FSHAPE}.{VFRELOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"{ISHAPE}.{VIUNOP}||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",
	"i8x16.popcnt||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",
	"i16x8.q15mulr_sat_s||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i32x4.dot_i8x16_s||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"{FSHAPE}.{VFUNOP}||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",
	"{ISHAPE}.{VITESTOP}||ResultType(OneResultType(V128ValType), OneResultType(I32ValType))",
	"{ISHAPE}.bitmask||ResultType(OneResultType(V128ValType), OneResultType(I32ValType))",
	"i8x16.narrow_i16x8_{SX}||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",
	"i16x8.narrow_i32x4_{SX}||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",
	"i16x8.extend_{HALF}_i8x16_{SX}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i32x4.extend_{HALF}_i16x8_{SX}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i64x2.extend_{HALF}_i32x4_{SX}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"{ISHAPE}.{VISHIFTOP}||ResultType(TwoResultTypes(V128ValType, I32ValType), OneResultType(V128ValType))",
	"{ISHAPE}.{VIBINOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i8x16.{VIMINMAXOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i16x8.{VIMINMAXOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i32x4.{VIMINMAXOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i8x16.{VISATBINOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i16x8.{VISATBINOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i8x16.mul||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i16x8.mul||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i32x4.mul||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i8x16.avgr_u||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i16x8.avgr_u||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i16x8.extmul_{HALF}_i8x16_{SX}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i32x4.extmul_{HALF}_i16x8_{SX}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i64x4.extmul_{HALF}_i32x4_{SX}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i16x8.extadd_pairwise_i8x16_{SX}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i32x4.extadd_pairwise_i16x8_{SX}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"{FSHAPE}.{VFBINOP}||ResultType(TwoResultTypes(V128ValType, V128ValType), OneResultType(V128ValType))",
	"i32x4.trunc_sat_f32x4_{SX}||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",
	"i32x4.trunc_sat_f64x2_{SX}_zero||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",
	"f32x4.convert_i32x4_{SX}||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",
	"f32x4.demote_f64x2_zero||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",
	"f64x2.convert_low_i32x4_{SX}||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",
	"f64x2.promote_low_f32x4||ResultType(OneResultType(V128ValType), OneResultType(V128ValType))",

	// Please see: https://webassembly.github.io/gc/core/syntax/instructions.html#reference-instructions
	"ref.null|(ht: HeapType)|ResultType(NoResultTypes, OneResultType(RefTypeValType(HeapTypeRefType(ht, True))))",
	"ref.func|(idx: FuncIDX)|ResultType(NoResultTypes, OneResultType(RefTypeValType(NullFuncRefType)))",
	"ref.is_null||todo",
	"ref.as_non_null||todo",
	"ref.eq||todo",
	"ref.test|(rt: RefType)|todo",
	"ref.cast|(rt: RefType)|todo",

	// Please see: https://webassembly.github.io/gc/core/syntax/instructions.html#aggregate-instructions
	"struct.new|(idx: TypeIDX)|ResultType(NoResultTypes, OneResultType(RefTypeValType(HeapTypeRefType(TypeIdxHeapType(idx), False))))",
	"struct.new_default|(idx: TypeIDX)|ResultType(NoResultTypes, OneResultType(RefTypeValType(HeapTypeRefType(TypeIdxHeapType(idx), False))))",
	"struct.get|(tidx: TypeIDX, fidx: FieldIDX)|todo",
	"struct.get_{SX}|(tidx: TypeIDX, fidx: FieldIDX)|todo",
	"struct.set|(tidx: TypeIDX, fidx: FieldIDX)|todo",
	"array.new|(idx: TypeIDX)|todo",
	"array.new_default|(idx: TypeIDX)|todo",
	"array.new_data|(idx: TypeIDX, data: DataIDX)|todo",
	"array.new_elem|(idx: TypeIDX, elem: ElemIDX)|todo",
	"array.get|(idx: TypeIDX)|todo",
	"array.get_{SX}|(idx: TypeIDX)|todo",
	"array.set|(idx: TypeIDX)|todo",
	"array.len||ResultType(NoResultTypes, OneResultType(I32ValType))",
	"array.fill|(idx: TypeIDX)|todo",
	"array.copy|(idx1: TypeIDX, idx2: TypeIDX)|todo",
	"array.init_data|(idx: TypeIDX, data: DataIDX)|todo",
	"array.init_elem|(idx: TypeIDX, elem: ElemIDX)|todo",
	"ref.i31||ResultType(NoResultTypes, OneResultType(RefTypeValType(I31RefType)))",
	"i31.get_{SX}||ResultType(NoResultTypes, OneResultType(I32ValType))",
	"any.convert_extern||ResultType(OneResultType(RefTypeValType(ExternRefType)), OneResultType(RefTypeValType(AnyRefType)))",
	"extern.convert_any||ResultType(OneResultType(RefTypeValType(AnyRefType)), OneResultType(RefTypeValType(ExternRefType)))",

	// Please see: https://webassembly.github.io/gc/core/syntax/instructions.html#parametric-instructions
	"drop||todo",
	"select||todo",
	"select_t|(vt: List(ValType))|todo",

	// Please see: https://webassembly.github.io/gc/core/syntax/instructions.html#variable-instructions
	"local.get|(idx: LocalIDX)|todo",
	"local.set|(idx: LocalIDX)|todo",
	"local.tee|(idx: LocalIDX)|todo",
	"global.get|(idx: GlobalIDX)|todo",
	"global.set|(idx: GlobalIDX)|todo",

	// Please see: https://webassembly.github.io/gc/core/syntax/instructions.html#table-instructions
	"table.get|(idx: TableIDX)|todo",
	"table.set|(idx: TableIDX)|todo",
	"table.size|(idx: TableIDX)|todo",
	"table.grow|(idx: TableIDX)|todo",
	"table.fill|(idx: TableIDX)|todo",
	"table.copy|(idx1: TableIDX, idx2: TableIDX)|todo",
	"table.init|(idx: TableIDX, elem: ElemIDX)|todo",
	"elem.drop|(idx: ElemIDX)|todo",

	// Please see: https://webassembly.github.io/gc/core/syntax/instructions.html#memory-instructions
	"i{NN}.load|(arg: MemArg)|ResultType(OneResultType(I32ValType), OneResultType(I{NN}ValType))",
	"f{NN}.load|(arg: MemArg)|ResultType(OneResultType(I32ValType), OneResultType(F{NN}ValType))",
	"v128.load|(arg: MemArg)|ResultType(OneResultType(I32ValType), OneResultType(V128ValType))",
	"i{NN}.store|(arg: MemArg)|ResultType(TwoResultTypes(I32ValType, I{NN}ValType), NoResultTypes)",
	"f{NN}.store|(arg: MemArg)|ResultType(TwoResultTypes(I32ValType, F{NN}ValType), NoResultTypes)",
	"v128.store|(arg: MemArg)|ResultType(TwoResultTypes(I32ValType, V128ValType), NoResultTypes)",
	"i{NN}.load{I32LOADSIZE}_{SX}|(arg: MemArg)|ResultType(OneResultType(I32ValType), OneResultType(I{NN}ValType))",
	"i64.load32_{SX}|(arg: MemArg)|ResultType(OneResultType(I32ValType), OneResultType(I64ValType))",
	"i{NN}.store{I32LOADSIZE}_{SX}|(arg: MemArg)|ResultType(TwoResultTypes(I32ValType, I{NN}ValType), NoResultTypes)",
	"i64.store32_{SX}|(arg: MemArg)|ResultType(TwoResultTypes(I32ValType, I64ValType), NoResultTypes)",
	"v128.load8x8_{SX}|(arg: MemArg)|ResultType(OneResultType(I32ValType), OneResultType(V128ValType))",
	"v128.load16x4_{SX}|(arg: MemArg)|ResultType(OneResultType(I32ValType), OneResultType(V128ValType))",
	"v128.load32x2_{SX}|(arg: MemArg)|ResultType(OneResultType(I32ValType), OneResultType(V128ValType))",
	"v128.load32_zero|(arg: MemArg)|ResultType(OneResultType(I32ValType), OneResultType(V128ValType))",
	"v128.load64_zero|(arg: MemArg)|ResultType(OneResultType(I32ValType), OneResultType(V128ValType))",
	"v128.load{WW}_splat|(arg: MemArg)|ResultType(OneResultType(I32ValType), OneResultType(V128ValType))",
	"v128.load8_lane|(arg: MemArg, idx: LaneIDX16)|ResultType(TwoResultTypes(I32ValType, V128ValType), OneResultType(V128ValType))",
	"v128.load16_lane|(arg: MemArg, idx: LaneIDX8)|ResultType(TwoResultTypes(I32ValType, V128ValType), OneResultType(V128ValType))",
	"v128.load32_lane|(arg: MemArg, idx: LaneIDX4)|ResultType(TwoResultTypes(I32ValType, V128ValType), OneResultType(V128ValType))",
	"v128.load64_lane|(arg: MemArg, idx: LaneIDX2)|ResultType(TwoResultTypes(I32ValType, V128ValType), OneResultType(V128ValType))",
	"v128.store8_lane|(arg: MemArg, idx: LaneIDX16)|ResultType(TwoResultTypes(I32ValType, V128ValType), NoResultTypes)",
	"v128.store16_lane|(arg: MemArg, idx: LaneIDX8)|ResultType(TwoResultTypes(I32ValType, V128ValType), NoResultTypes)",
	"v128.store32_lane|(arg: MemArg, idx: LaneIDX4)|ResultType(TwoResultTypes(I32ValType, V128ValType), NoResultTypes)",
	"v128.store64_lane|(arg: MemArg, idx: LaneIDX2)|ResultType(TwoResultTypes(I32ValType, V128ValType), NoResultTypes)",
	"memory.size||ResultType(NoResultTypes, OneResultType(I32ValType))",
	"memory.grow||ResultType(OneResultType(I32ValType), OneResultType(I32ValType))",
	"memory.fill||ResultType(ThreeResultTypes(I32ValType, I32ValType, I32ValType), NoResultTypes)",
	"memory.copy||ResultType(ThreeResultTypes(I32ValType, I32ValType, I32ValType), NoResultTypes)",
	"memory.init|(idx: DataIDX)|ResultType(ThreeResultTypes(I32ValType, I32ValType, I32ValType), NoResultTypes)",
	"data.drop|(idx: DataIDX)|ResultType(NoResultTypes, NoResultTypes)",

	// https://webassembly.github.io/gc/core/syntax/instructions.html#control-instructions
	"nop||todo",
	"unreachable||todo",
	"block||todo",
	"loop||todo",
	"if||todo",
	"br||todo",
	"br_if||todo",
	"br_table||todo",
	"br_on_null||todo",
	"br_on_non_null||todo",
	"br_on_cast||todo",
	"br_on_cast_fail||todo",
	"return||todo",
	"call||todo",
	"call_ref||todo",
	"call_indirect||todo",
	"return_call||todo",
	"return_call_ref||todo",
	"return_call_indirect||todo",
];

type Context = {
	[key: string]: string;
};

const pattern = /\{([A-Z0-9]+)\}/i;
const fork = (list: string[], context: Context): string[] => {
	// always make a copy, so we don't mutate the original
	const workingList = [...list];
	let index = 0;
	while (true) {
		if (index >= workingList.length) break;

		const input = workingList[index];
		const match = input.match(pattern);
		if (match) {
			const key = match[1];
			// if we have a match, and the match is in the context,
			if (key in context) {
				// key is in the context
				const replacement = context[key];
				// do an inline replacement, because this pattern has already been decided
				const result = input.replace(pattern, replacement);

				// console.log("replacing", input, "using", key, "into", result);
				workingList[index] = result;
			} else {
				// for each item in the pattern list, fork the list
				const pattern = patterns[key];
				if (!pattern) throw new Error(`Missing pattern: ${key}`);

				// remove the current item from the list, and then fork a list of only that item
				const target = workingList.splice(index, 1);
				// console.log("target is", target);

				for (const element of pattern) {
					// create a new context with the value set
					const nextContext = { ...context, [key]: element };

					// console.log("forking", target, "with", nextContext);

					const result = fork(target, nextContext);
					// console.log("forking", target, "with", nextContext, "became", result);

					// pushing them to the end is faster than inserting them inline
					workingList.splice(index, 0, ...result);
				}
			}
		} else {
			index++;
		}
	}
	return workingList;
};

const result = fork(instrs, {});

await Bun.write(
	"./types.txt",
	`
pub type Instruction {
${result
	.map((e) => {
		const [name, immediates, _] = e.split("|");
		const nameText = pascalCase(name.replace(".", "_"));
		return `  ${nameText}${immediates}`;
	})
	.join("\n")}
}\n`,
);
console.log("Wrote types");

const resultsLines = [] as string[];

const caseMap = new Map<string, Set<string>>();

for (const e of result) {
	const [name, immediates, results] = e.split("|");
	const nameText = pascalCase(name.replace(".", "_"));
	const immediatesText =
		immediates === ""
			? ""
			: `(${immediates
					.split(",")
					.map((a) => "_")
					.join(", ")})`;
	const left = `${nameText}${immediatesText}`;

	const set = caseMap.get(results) ?? new Set<string>();
	set.add(left);

	caseMap.set(results, set);
}

await Bun.write(
	"./results.txt",
	`
pub fn get_result_type(instruction: Instruction) -> ResultType {
  case instruction {
${Array.from(caseMap.entries())
	.map(([k, v]) => `    ${Array.from(v).join(" | ")} -> ${k}`)
	.join("\n")}
  }
}\n`,
);
console.log("Wrote results");
