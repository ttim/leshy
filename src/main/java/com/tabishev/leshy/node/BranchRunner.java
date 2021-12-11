package com.tabishev.leshy.node;

import com.tabishev.leshy.runtime.StackMemory;

public class BranchRunner extends Runner {
    public final RunnerCtx ctx;
    public final Node.Branch node;

    private final Impl impl;
    private Runner ifTrue;
    private Runner ifFalse;

    public BranchRunner(RunnerCtx ctx, Node.Branch node) {
        this.ctx = ctx;
        this.node = node;
        this.impl = Runners.condition(node.condition());
    }

    @Override
    public RunnerCtx ctx() {
        return ctx;
    }

    @Override
    public Node node() {
        return node;
    }

    @Override
    public void refresh() {
        if (ifTrue != null) ifTrue = ctx.create(node.ifTrue());
        if (ifFalse != null) ifFalse = ctx.create(node.ifFalse());
    }

    @Override
    public Runner runInternal(StackMemory stack) {
        if (impl.run(stack)) {
            if (ifTrue == null) ifTrue = ctx.create(node.ifTrue());
            return ifTrue;
        } else {
            if (ifFalse == null) ifFalse = ctx.create(node.ifFalse());
            return ifFalse;
        }
    }

    public static abstract class Impl {
        abstract boolean run(StackMemory stack);
    }
}
