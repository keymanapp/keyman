package com.tavultesoft.kmea.data.adapters;

import android.content.Context;
import android.database.DataSetObserver;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collection;
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
  final List<Element> filteredList;

  boolean isMutating = false;

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
      } else if(listener.isMutating){
        // This event was triggered by a reaction to what our owning listener did.  No need to update.
        return;
      }
      listener.setNotifyOnChange(false); // Disable event notifications temporarily.
      listener._internalClear();;
      listener._internalAddAll(NestedAdapter.getFilteredElements(listener.wrappedAdapter, listener.filter));
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
    this(context, resource, adapter, filter, getFilteredElements(adapter, filter));
  }

  public NestedAdapter(@NonNull Context context, int resource, @NonNull A adapter, AdapterFilter<Element> filter, List<Element> filteredList) {
    super(context, resource, getFilteredElements(adapter, filter));

    this.wrappedAdapter = adapter;
    observer = new WrapperObserver<>(this, adapter);
    this.wrappedAdapter.registerDataSetObserver(observer);

    this.filter = filter;
    this.filteredList = filteredList;
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

  @Override
  public void add(Element elem) {
    super.add(elem); // Modifies our internal element list, which is separate from the wrapped adapter's.

    this.isMutating = true;
    wrappedAdapter.add(elem); // Reflect the changes to our wrapped adapter.
    this.isMutating = false;
  }

  @Override
  public void addAll(Element... elements) {
    super.addAll(elements);

    this.isMutating = true;
    wrappedAdapter.addAll(elements);
    this.isMutating = false;
  }

  @Override
  public void addAll(@NonNull Collection<? extends Element> collection) {
    super.addAll(collection);

    this.isMutating = true;
    wrappedAdapter.addAll(collection);
    this.isMutating = false;
  }

  @Override
  public void remove(Element element) {
    super.remove(element);

    this.isMutating = true;
    wrappedAdapter.remove(element);
    this.isMutating = false;
  }

  @Override
  public void clear() {
    List<Element> cleared = new ArrayList<>(filteredList); // Duplicate list before it's cleared.
    super.clear();

    // Since we use a filtered view for clear() and we want the clear() operation to seem atomic,
    // we need to manage the setNotify flags.  This same method is also used to manage this class,
    // though, so `doNotify` signals if this is a reactive clear() triggered by our wrapped adapter
    // and prevents accidental looping.
    this.isMutating = true;
    wrappedAdapter.setNotifyOnChange(false);

    for(Element element: cleared) {
      wrappedAdapter.remove(element);
    }


    wrappedAdapter.notifyDataSetChanged();
    this.isMutating = false;
  }

  // These methods allow bypassing of our externally-visible 'linking' versions for operations
  // triggered by the other end of the link.
  protected void _internalAddAll(@NonNull Collection<? extends Element> collection) {
    super.addAll(collection);
  }

  protected void _internalClear() {
    super.clear();
  }

  // TODO:  As needed, override mutation functions so that we can reflect the changes onto
  //        the wrappedAdapter instance.  We should handle these cases directly rather than
  //        through self-triggered events, as we may only hold a subset due to use of filters.
}
