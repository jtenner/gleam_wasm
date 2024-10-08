import gleam/option.{type Option, None, Some}
import internal/finger_tree.{type FingerTree}
import internal/structure/types.{
  type BlockType, type Expr, type Instruction, Block, Expr, If, Loop,
}

type BlockDefinition {
  EmptyTop
  OuterMostBlock(instructions: FingerTree(Instruction))
  IfBlock(bt: BlockType, instructions: FingerTree(Instruction))
  ElseBlock(
    bt: BlockType,
    if_instructions: FingerTree(Instruction),
    else_instructions: FingerTree(Instruction),
  )
  LoopBlock(bt: BlockType, instructions: FingerTree(Instruction))
  InlineBlock(bt: BlockType, instructions: FingerTree(Instruction))
}

pub opaque type ExpressionBuilder {
  ExpressionBuilder(
    result: Option(Expr),
    top: BlockDefinition,
    block_stack: FingerTree(BlockDefinition),
  )
}

pub fn new() -> ExpressionBuilder {
  ExpressionBuilder(None, OuterMostBlock(finger_tree.empty), finger_tree.empty)
}

pub fn begin_if(builder: ExpressionBuilder, bt: BlockType) -> ExpressionBuilder {
  let ExpressionBuilder(result, top, block_stack) = builder
  ExpressionBuilder(
    result,
    IfBlock(bt, finger_tree.empty),
    block_stack
      |> finger_tree.push(top),
  )
}

pub fn begin_else(builder: ExpressionBuilder) {
  case builder.top {
    IfBlock(bt, instructions) ->
      ExpressionBuilder(
        ..builder,
        top: ElseBlock(bt, instructions, finger_tree.empty),
      )
    EmptyTop -> panic as "Stack state underflow"
    _ -> panic as "Invalid stack state, cannot start else block"
  }
}

pub fn begin_loop(
  builder: ExpressionBuilder,
  bt: BlockType,
) -> ExpressionBuilder {
  ExpressionBuilder(
    builder.result,
    LoopBlock(bt, finger_tree.empty),
    builder.block_stack
      |> finger_tree.push(builder.top),
  )
}

pub fn begin_block(
  builder: ExpressionBuilder,
  bt: BlockType,
) -> ExpressionBuilder {
  ExpressionBuilder(
    builder.result,
    InlineBlock(bt, finger_tree.empty),
    builder.block_stack
      |> finger_tree.push(builder.top),
  )
}

pub fn end(builder: ExpressionBuilder) {
  case builder {
    ExpressionBuilder(None, OuterMostBlock(instructions), block_stack)
      if block_stack == finger_tree.empty
    -> Ok(ExpressionBuilder(Some(Expr(instructions)), EmptyTop, block_stack))
    ExpressionBuilder(None, IfBlock(bt, if_instructions), block_stack) ->
      do_append(block_stack, If(bt, if_instructions, None))
    ExpressionBuilder(
      None,
      ElseBlock(bt, if_instructions, else_instructions),
      block_stack,
    ) ->
      do_append(block_stack, If(bt, if_instructions, Some(else_instructions)))
    ExpressionBuilder(None, LoopBlock(bt, loop_instructions), block_stack) ->
      do_append(block_stack, Loop(bt, Expr(loop_instructions)))
    ExpressionBuilder(None, InlineBlock(bt, block_instructions), block_stack) ->
      do_append(block_stack, Block(bt, Expr(block_instructions)))
    ExpressionBuilder(_, EmptyTop, _) -> Error("Stack state underflow")
    _ -> Error("Invalid stack state")
  }
}

fn do_append(
  next_stack: FingerTree(BlockDefinition),
  next_instruction: Instruction,
) {
  case next_stack |> finger_tree.pop() {
    Ok(#(OuterMostBlock(outer_instructions), next_stack)) ->
      Ok(ExpressionBuilder(
        None,
        OuterMostBlock(
          outer_instructions
          |> finger_tree.push(next_instruction),
        ),
        next_stack,
      ))

    Ok(#(IfBlock(outer_bt, outer_if_instructions), next_stack)) ->
      Ok(ExpressionBuilder(
        None,
        IfBlock(
          outer_bt,
          outer_if_instructions |> finger_tree.push(next_instruction),
        ),
        next_stack,
      ))
    Ok(#(
      ElseBlock(outer_bt, outer_if_instructions, outer_else_instructions),
      next_stack,
    )) ->
      Ok(ExpressionBuilder(
        None,
        ElseBlock(
          outer_bt,
          outer_if_instructions,
          outer_else_instructions |> finger_tree.push(next_instruction),
        ),
        next_stack,
      ))

    Ok(#(LoopBlock(outer_bt, outer_instructions), next_stack)) ->
      Ok(ExpressionBuilder(
        None,
        LoopBlock(
          outer_bt,
          outer_instructions |> finger_tree.push(next_instruction),
        ),
        next_stack,
      ))

    Ok(#(InlineBlock(outer_bt, outer_instructions), next_stack)) ->
      Ok(ExpressionBuilder(
        None,
        InlineBlock(
          outer_bt,
          outer_instructions |> finger_tree.push(next_instruction),
        ),
        next_stack,
      ))
    _ -> Error("Stack state underflow")
  }
}

pub fn push(builder: ExpressionBuilder, inst: Instruction) {
  case builder.top {
    OuterMostBlock(instructions) ->
      ExpressionBuilder(
        None,
        OuterMostBlock(instructions |> finger_tree.push(inst)),
        builder.block_stack,
      )
    IfBlock(bt, instructions) ->
      ExpressionBuilder(
        None,
        IfBlock(bt, instructions |> finger_tree.push(inst)),
        builder.block_stack,
      )
    ElseBlock(bt, if_instructions, else_instructions) ->
      ExpressionBuilder(
        None,
        ElseBlock(
          bt,
          if_instructions,
          else_instructions |> finger_tree.push(inst),
        ),
        builder.block_stack,
      )
    LoopBlock(bt, instructions) ->
      ExpressionBuilder(
        None,
        LoopBlock(bt, instructions |> finger_tree.push(inst)),
        builder.block_stack,
      )
    InlineBlock(bt, instructions) ->
      ExpressionBuilder(
        None,
        InlineBlock(bt, instructions |> finger_tree.push(inst)),
        builder.block_stack,
      )
    _ -> panic as "Expression completed, cannot push instruction"
  }
}

pub fn end_unwrap(builder) {
  case builder {
    ExpressionBuilder(None, OuterMostBlock(instructions), stack)
      if stack == finger_tree.empty
    -> Expr(instructions)
    _ -> panic as "Unable to unwrap, invalid expression state"
  }
}
