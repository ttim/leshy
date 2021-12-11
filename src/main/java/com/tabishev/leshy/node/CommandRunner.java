package com.tabishev.leshy.node;

import com.tabishev.leshy.runtime.StackMemory;

public class CommandRunner extends Runner {
    public final RunnerCtx ctx;
    public final Node.Run node;
    public final Impl impl;

    private Runner next;

    public CommandRunner(RunnerCtx ctx, Node.Run node) {
        this.ctx = ctx;
        this.node = node;
        this.impl = Runners.command(node.command());
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
        if (next != null) next = ctx.create(node.next());
    }

    @Override
    public Runner runInternal(StackMemory stack) {
        impl.run(stack);
        if (next == null) next = ctx.create(node.next());
        return next;
    }

    public static abstract class Impl {
        public abstract void run(StackMemory stack);
    }
}
