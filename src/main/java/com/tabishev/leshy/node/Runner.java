package com.tabishev.leshy.node;

import com.tabishev.leshy.runtime.StackMemory;

public abstract class Runner {
    private static final boolean debug = false;

    // todo: remove
    public abstract RunnerCtx ctx();
    // todo: remove
    public abstract Node node();

    public abstract void refresh();

    public abstract Runner runInternal(StackMemory stack);

    public final FinalRunner runFully(StackMemory stack) {
        Runner runner = this;
        while (!(runner instanceof FinalRunner)) {
            debug("start run");
            var nextRunner = runner.runInternal(stack);
            debug("finish run");
            runner = nextRunner;
        }
        return (FinalRunner) runner;
    }

    private void debug(String msg) {
        if (debug) System.out.println(toString() + ": " + msg);
    }

    public static Runner create(RunnerCtx ctx, Node node) {
        if (node instanceof Node.Run run) {
            return new CommandRunner(ctx, run);
        } else if (node instanceof Node.Branch branch) {
            return new BranchRunner(ctx, branch);
        } else if (node instanceof Node.Call call) {
            return new CallRunner(ctx, call);
        } else if (node instanceof Node.Final finalNode) {
            return new FinalRunner(ctx, finalNode);
        } else {
            throw new IllegalArgumentException(node.toString());
        }
    }
}
