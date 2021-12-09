package com.tabishev.leshy.bytecode;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public interface Statement {
    void write(MethodVisitor writer);

    static Statement expression(Expression expr) {
        return writer -> {
            var kind = expr.push(writer);
            kind.popInst.ifPresent(pop -> writer.visitInsn(pop));
        };
    }

    static Statement ret(Expression expr) {
        return writer -> {
            var kind = expr.push(writer);
            writer.visitInsn(kind.retInst);
        };
    }

    static Statement store(int idx, Expression expr) {
        return writer -> {
            var kind = expr.push(writer);
            writer.visitVarInsn(kind.storeInst.get(), idx);
        };
    }

    static Statement branch(Expression left, BranchModifier modifier, Expression right, Label label) {
        return writer -> {
            var leftKind = left.push(writer);
            var rightKind = right.push(writer);
            assert leftKind == rightKind;
            switch (leftKind) {
                case Int -> writer.visitJumpInsn(modifier.intOpcode, label);
                case Long -> {
                    writer.visitInsn(Opcodes.LCMP);
                    writer.visitJumpInsn(modifier.cmpOpcode, label);
                }
                case Void -> throw new UnsupportedOperationException();
                case Object -> throw new UnsupportedOperationException();
            }
        };
    }

    static Statement branch(Expression boolArg, Label label) {
        return writer -> {
            var kind = boolArg.push(writer);
            assert kind == ExpressionKind.Int;
            writer.visitJumpInsn(Opcodes.IFGT, label);
        };
    }
}
