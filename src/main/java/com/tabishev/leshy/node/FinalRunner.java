package com.tabishev.leshy.node;

import com.tabishev.leshy.runtime.StackMemory;

public class FinalRunner extends Runner {
    public final RunnerCtx ctx;
    public final Node.Final node;

    public FinalRunner(RunnerCtx ctx, Node.Final node) {
        this.ctx = ctx;
        this.node = node;
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
    public void refresh() {}

    @Override
    public Runner runInternal(StackMemory stack) {
        throw new IllegalStateException();
    }
}
