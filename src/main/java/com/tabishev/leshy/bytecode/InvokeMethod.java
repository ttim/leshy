package com.tabishev.leshy.bytecode;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class InvokeMethod implements Expression {
    final int opcode;
    final Class<?> clazz;
    final String name;
    final Iterable<Expression> args;

    public InvokeMethod(int opcode, Class<?> clazz, String name, Iterable<Expression> args) {
        this.opcode = opcode;
        this.clazz = clazz;
        this.name = name;
        this.args = args;
    }

    @Override
    public ExpressionKind push(MethodVisitor writer) {
        for (Expression arg : args) {
            var pushed = arg.push(writer);
            assert pushed != ExpressionKind.Void;
        }
        writer.visitMethodInsn(opcode, Type.getType(clazz).getInternalName(), name, Type.getMethodDescriptor(method()), false);
        return ExpressionKind.of(Type.getType(method().getReturnType()));
    }

    private Method method() {
        var methods = Arrays.stream(clazz.getMethods()).filter(method -> method.getName() == name).collect(Collectors.toList());
        assert methods.size() == 1;
        return methods.get(0);
    }

    public static InvokeMethod invokeStatic(Class<?> clazz, String name, Expression... args) {
        return new InvokeMethod(Opcodes.INVOKESTATIC, clazz, name, Arrays.asList(args));
    }

    public static InvokeMethod invokeVirtual(Class<?> clazz, String name, Expression... args) {
        return new InvokeMethod(Opcodes.INVOKEVIRTUAL, clazz, name, Arrays.asList(args));
    }
}
