package forth.virtual.machine

sealed trait ByteCode

case object FPush extends ByteCode

case object FPlus extends ByteCode

case object FSub extends ByteCode

case object FBranch extends ByteCode

case object FReturn extends ByteCode

case object FSetMode extends ByteCode

case object FGetMode extends ByteCode

case object FAlloc extends ByteCode

case object FSetDict extends ByteCode

case object FGetDict extends ByteCode

case object FGetField extends ByteCode

case object FSetField extends ByteCode

case object FCall extends ByteCode

// Should this be a bytecode?
case object FLookup extends ByteCode

case object FInWord extends ByteCode

case object FInChar extends ByteCode

case object FIsInt extends ByteCode

case object FConvertInt extends ByteCode

case object FConcat extends ByteCode

case object FPrint extends ByteCode

case object FDrop extends ByteCode

case object FDup extends ByteCode

case object FSwap extends ByteCode

case object FPushCont extends ByteCode

case object FPopCont extends ByteCode

case object FPeekCont extends ByteCode