package com.tavultesoft.kmea.data.adapters;

import android.content.Context;
import android.database.DataSetObserver;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

/**
 * An ArrayAdapter subclass that uses another ArrayAdapter (or subclass) as its data source,
 * propagating relevant changes across both to maintain a 'linked'/'nested' state.
 * Also permits use of an AdapterFilter to allow using a filtered view of that source.
 * @param <Element> The shared item type of the two Adapters.
 * @param <A> The type specification of the Adapter to be nested/linked.
 */
public class NestedAdapter<Element, A extends ArrayAdapter<Element> & ListBacked<Element>> extends ArrayAdapter<Element>  {
  final A wrappedAdapter;
  final AdapterFilter<Element> filter;

  /**
   * Links a NestedAdapter 'listener' with a pre-existing ArrayAdapter 'source' by listening to its
   * events that signal state changes.  Is GC-friendly, automatically disconnecting and managing
   * itself when the 'listener' is GC'd.
   * @param <E> The Element type of the Adapters being linked.
   * @param <S> The type of the 'wrapped' source ArrayAdapter (possibly a subclass).
   * @param <A> The type of the 'listener' NestedAdapter, complete with its generic type parameters.
   */
  static class WrapperObserver<E, S extends ArrayAdapter<E> & ListBacked<E>, A extends NestedAdapter<E, S>> extends DataSetObserver {
    // By being static and using a WeakReference here, we avoid memory leaks that would otherwise
    // prevent our owner from being GC'd.
    private WeakReference<A> listenerRef;
    private S source;

    WrapperObserver(A listener, S source) {
      this.listenerRef = new WeakReference<>(listener);
      this.source = source;
    }

    @Override
    public void onChanged() {
      A listener = listenerRef.get();
      if(listener == null) {
        // Our listener has been GC'd.  Time to disconnect and allow this instance to be GC'd, too.
        source.unregisterDataSetObserver(this);
        return;
      }
      listener.setNotifyOnChange(false); // Disable event notifications temporarily.
      listener.clear();
      listener.addAll(NestedAdapter.getFilteredElements(listener.wrappedAdapter, listener.filter));
      listener.notifyDataSetChanged();  // Re-enables events and signals that we did change.
    }

    @Override
    public void onInvalidated() {
      A listener = listenerRef.get();
      if(listener == null) {
        // Our listener has been GC'd.  Time to disconnect and allow this instance to be GC'd, too.
        source.unregisterDataSetObserver(this);
        return;
      }
      listener.notifyDataSetInvalidated();
    }
  }

  private final WrapperObserver<Element, A, NestedAdapter<Element, A>> observer;

  public NestedAdapter(@NonNull Context context, int resource, @NonNull A adapter) {
    this(context, resource, adapter, new AdapterFilter<Element>() {
        @Override
        public boolean matches(Element elem) {
            return true;
        }
    });
  }

  public NestedAdapter(@NonNull Context context, int resource, @NonNull A adapter, AdapterFilter<Element> filter) {
    super(context, resource, getFilteredElements(adapter, filter));

    this.wrappedAdapter = adapter;
    observer = new WrapperObserver<>(this, adapter);
    this.wrappedAdapter.registerDataSetObserver(observer);

    this.filter = filter;
  }

  protected static <Element, A extends ArrayAdapter<Element> & ListBacked<Element>> List<Element> getFilteredElements(A adapter, AdapterFilter<Element> filter) {
    ArrayList<Element> list = new ArrayList<>();

    for(Element elem: adapter.asList()) {
      if(filter.matches(elem)) {
        list.add(elem);
      }
    }

    return list;
  }

  @Override
  public void notifyDataSetInvalidated() {
    this.wrappedAdapter.unregisterDataSetObserver(this.observer);
  }

  // TODO:  As needed, override mutation functions so that we can reflect the changes onto
  //        the wrappedAdapter instance.  We should handle these cases directly rather than
  //        through self-triggered events, as we may only hold a subset due to use of filters.
  //
  //        Management of setNotifyOnChange() on the wrappedAdapter is recommended to bypass event
  //        notification during such edits.
}
