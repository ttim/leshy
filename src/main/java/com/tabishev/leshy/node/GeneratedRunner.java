package com.tabishev.leshy.node;

import java.lang.reflect.Field;

public abstract class GeneratedRunner extends Runner {
    @Override
    public void refresh() {
        try {
            for (Field field : this.getClass().getDeclaredFields()) {
                if (field.getType().isAssignableFrom(Runner.class)) {
                    var prev = (Runner) field.get(this);
                    field.set(this, prev.ctx().create(prev.node()));
                }
            }
        } catch (IllegalAccessException ignore) {}
    }
}
