package com.tabishev.leshy.bytecode;

import org.objectweb.asm.Opcodes;

public enum BranchModifier {
    GT(Opcodes.IF_ICMPGT, Opcodes.IFGT),
    LE(Opcodes.IF_ICMPLE, Opcodes.IFLE),
    EQ(Opcodes.IF_ICMPEQ, Opcodes.IFEQ);

    // int1 :op: int2
    final int intOpcode;
    // int :op: 0
    final int cmpOpcode;

    BranchModifier(int intOpcode, int cmpOpcode) {
        this.intOpcode = intOpcode;
        this.cmpOpcode = cmpOpcode;
    }
}
