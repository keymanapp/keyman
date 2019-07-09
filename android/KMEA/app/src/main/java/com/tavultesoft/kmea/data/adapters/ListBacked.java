package com.tavultesoft.kmea.data.adapters;

import java.util.List;

public interface ListBacked<T> {
    List<T> asList();
}