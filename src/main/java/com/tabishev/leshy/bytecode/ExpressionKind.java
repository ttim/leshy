package com.tabishev.leshy.bytecode;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import java.util.Optional;

public enum ExpressionKind {
    Void(Opcodes.RETURN, Optional.empty(), Optional.empty(), Optional.empty(),
            Optional.empty(), Optional.empty(), Optional.empty()),
    Object(Opcodes.ARETURN, Optional.of(Opcodes.POP), Optional.of(Opcodes.ALOAD), Optional.of(Opcodes.ASTORE),
            Optional.empty(), Optional.empty(), Optional.empty()),
    Int(Opcodes.IRETURN, Optional.of(Opcodes.POP), Optional.of(Opcodes.ILOAD), Optional.of(Opcodes.ISTORE),
            Optional.of(Opcodes.IADD), Optional.of(Opcodes.IMUL), Optional.of(Opcodes.INEG)),
    Long(Opcodes.LRETURN, Optional.of(Opcodes.POP2), Optional.of(Opcodes.LLOAD), Optional.of(Opcodes.LSTORE),
            Optional.of(Opcodes.LADD), Optional.of(Opcodes.LMUL), Optional.of(Opcodes.LNEG));

    public final int retInst;
    public final Optional<Integer> popInst;

    public final Optional<Integer> loadInst;
    public final Optional<Integer> storeInst;

    public final Optional<Integer> sumInst;
    public final Optional<Integer> multInst;
    public final Optional<Integer> negateInst;

    ExpressionKind(int retInst, Optional<Integer> popInst, Optional<Integer> loadInst, Optional<Integer> storeInst,
         Optional<Integer> sumInst, Optional<Integer> multInst, Optional<Integer> negateInst) {
        this.retInst = retInst;
        this.popInst = popInst;
        this.loadInst = loadInst;
        this.storeInst = storeInst;
        this.sumInst = sumInst;
        this.multInst = multInst;
        this.negateInst = negateInst;
    }

    public static ExpressionKind of(Type tpe) {
        return switch (tpe.getSort()) {
            case Type.VOID -> ExpressionKind.Void;
            case Type.OBJECT -> ExpressionKind.Object;
            case Type.INT -> ExpressionKind.Int;
            case Type.LONG -> ExpressionKind.Long;
            case Type.BOOLEAN -> ExpressionKind.Int;
            default -> throw new IllegalArgumentException(tpe.toString());
        };
    }
}
