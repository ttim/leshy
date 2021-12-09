package com.tabishev.leshy.bytecode;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import java.util.function.Consumer;
import java.util.function.Function;

public interface Expression {
    ExpressionKind push(MethodVisitor writer);

    static Expression voidExpression() {
        return new SimpleExpression(ExpressionKind.Void, writer -> {});
    }

    static Expression constExpression(int value) {
        return constExpression(ExpressionKind.Int, value);
    }

    static Expression constExpression(long value) {
        return constExpression(ExpressionKind.Long, value);
    }

    static Expression constExpression(boolean value) {
        if (value) {
            return constExpression(ExpressionKind.Int, 1);
        } else {
            return constExpression(ExpressionKind.Int, 0);
        }
    }

    static Expression negate(Expression arg) {
        return new UnaryExpression(arg, kind -> kind.negateInst.get());
    }

    static Expression sum(Expression arg1, Expression arg2) {
        return new BinaryExpression(arg1, arg2, kind -> kind.sumInst.get());
    }

    static Expression mult(Expression arg1, Expression arg2) {
        return new BinaryExpression(arg1, arg2, kind -> kind.multInst.get());
    }

    static Expression local(int idx, ExpressionKind kind) {
        return new SimpleExpression(kind, writer -> writer.visitVarInsn(kind.loadInst.get(), idx));
    }

    static Expression thisInstance() {
        return new SimpleExpression(ExpressionKind.Object, writer -> writer.visitVarInsn(Opcodes.ALOAD, 0));
    }

    static Expression invokeSuper(Class<?> superClass) {
        return writer -> {
            writer.visitVarInsn(Opcodes.ALOAD, 0);
            writer.visitMethodInsn(Opcodes.INVOKESPECIAL, Type.getInternalName(superClass), "<init>", "()V", false);
            return ExpressionKind.Void;
        };
    }

    static Expression cast(Expression expression, Class<?> clz) {
        return writer -> {
            var kind = expression.push(writer);
            writer.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(clz));
            return kind;
        };
    }

    private static Expression constExpression(ExpressionKind kind, Object value) {
        return new SimpleExpression(kind, writer -> writer.visitLdcInsn(value));
    }
}

// todo: use records for this classes
class UnaryExpression implements Expression {
    final Expression arg;
    final Function<ExpressionKind, Integer> instruction;

    UnaryExpression(Expression arg, Function<ExpressionKind, Integer> instruction) {
        this.arg = arg;
        this.instruction = instruction;
    }

    @Override
    public ExpressionKind push(MethodVisitor writer) {
        var kind = arg.push(writer);
        writer.visitInsn(instruction.apply(kind));
        return kind;
    }
}

class BinaryExpression implements Expression {
    final Expression arg1;
    final Expression arg2;
    final Function<ExpressionKind, Integer> instruction;

    BinaryExpression(Expression arg1, Expression arg2, Function<ExpressionKind, Integer> instruction) {
        this.arg1 = arg1;
        this.arg2 = arg2;
        this.instruction = instruction;
    }

    @Override
    public ExpressionKind push(MethodVisitor writer) {
        var kind1 = arg1.push(writer);
        var kind2 = arg2.push(writer);
        assert kind1 == kind2;
        writer.visitInsn(instruction.apply(kind1));
        return kind1;
    }
}

class SimpleExpression implements Expression {
    final ExpressionKind kind;
    final Consumer<MethodVisitor> push;

    public SimpleExpression(ExpressionKind kind, Consumer<MethodVisitor> push) {
        this.kind = kind;
        this.push = push;
    }

    @Override
    public ExpressionKind push(MethodVisitor writer) {
        push.accept(writer);
        return kind;
    }
}
