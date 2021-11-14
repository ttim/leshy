package com.tabishev.leshy.compiler;

import com.tabishev.leshy.runtime.FrameOffset;

public class Interop {
    public static FrameOffset frameOffset(int offset) {
        return new FrameOffset(offset);
    }
}
