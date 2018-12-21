package impl.bytecode

import impl.common.Id


package object implementation {
  implicit object CompilableIsCompilable extends IsCompilable[Compilable, Procedure] {
    override def getId[A: Compilable]: Procedure = implicitly[Compilable[A]].id
  }

  implicit object LabelIsLabel extends IsLabel[Label, Id] {
    override def newLabel(): Id[Label] = Label.newLabel()
  }

  implicit object ProcedureIsProcedure extends IsProcedure[Procedure, Relation, Compilable, Set] {
    override def convertObject[A: Compilable](a: A): Procedure =
      implicitly[Compilable[A]].toProc(a)

    override def convertFindable[A: Compilable](fa: Set[A]): Procedure =
      implicitly[Compilable[A]].toProc(fa)

    override def convertRelation[A: Compilable, B: Compilable](r: Relation[A, B]): Procedure =
      r.procedure
  }
}
