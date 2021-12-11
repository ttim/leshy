package com.tabishev.leshy.node;

import com.tabishev.leshy.runtime.FrameOffset;
import com.tabishev.leshy.runtime.StackMemory;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class CallRunner extends Runner {
    public final RunnerCtx ctx;
    public final Node.Call node;
    public final int offset;

    private Runner call;
    public final Map<Node.Final, Runner> next = new HashMap<>();

    public CallRunner(RunnerCtx ctx, Node.Call node) {
        this.ctx = ctx;
        this.node = node;
        this.offset = node.offset().get();
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
        if (call != null) call = ctx.create(node.call());
        var replaces = next.keySet().stream().collect(Collectors.toMap(
                finalNode -> finalNode,
                finalNode -> ctx.create(node.next(finalNode))
        ));
        next.putAll(replaces);
    }

    @Override
    public final Runner runInternal(StackMemory stack) {
        stack.moveFrame(offset);
        if (call == null) call = ctx.create(node.call());
        FinalRunner finalRunner = call.runFully(stack);
        stack.moveFrame(-offset);
        return nextRunner(finalRunner);
    }

    public final Runner nextRunner(FinalRunner finalRunner) {
        Runner nextRunner = next.get(finalRunner.node);
        if (nextRunner == null) {
            nextRunner = ctx.create(node.next(finalRunner.node));
            next.put(finalRunner.node, nextRunner);
        }
        return nextRunner;
    }
}
