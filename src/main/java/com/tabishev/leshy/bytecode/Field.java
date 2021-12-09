package com.tabishev.leshy.bytecode;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class Field {
    final boolean isStatic;
    final String name;
    final Type owner;
    final Type tpe;

    public Field(boolean isStatic, String name, Type owner, Type tpe) {
        this.isStatic = isStatic;
        this.name = name;
        this.owner = owner;
        this.tpe = tpe;
    }

    public Expression get(Expression object) {
        assert !isStatic;
        return writer -> {
            object.push(writer);
            writer.visitFieldInsn(Opcodes.GETFIELD, owner.getInternalName(), name, tpe.getDescriptor());
            return ExpressionKind.of(tpe);
        };
    };

    // todo: seems like I need to create "terminate" expression which represents void kind!
    public void put(MethodVisitor writer, Expression object, Expression value) {
        assert !isStatic;
        object.push(writer);
        value.push(writer);
        writer.visitFieldInsn(Opcodes.PUTFIELD, owner.getInternalName(), name, tpe.getDescriptor());
    }
}
