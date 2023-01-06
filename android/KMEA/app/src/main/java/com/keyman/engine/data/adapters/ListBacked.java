package com.keyman.engine.data.adapters;

import java.util.List;

public interface ListBacked<T> {
    List<T> asList();
}