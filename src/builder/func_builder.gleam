import internal/finger_tree.{type FingerTree}
import internal/structure/types.{type DefType, type Instruction, type LocalType}

type BlockDefinition {
  OuterMostBlock(instructions: FingerTree(Instruction))
  IfBlock(instructions: FingerTree(Instruction))
  ElseBlock(instructions: FingerTree(Instruction))
  LoopBlock(instructions: FingerTree(Instruction))
  Block(instructions: FingerTree(Instruction))
}

pub opaque type FuncBuilder {
  FuncBuilder(
    // context: Context,
    block_stack: FingerTree(BlockDefinition),
    locals: FingerTree(LocalType),
  )
}
