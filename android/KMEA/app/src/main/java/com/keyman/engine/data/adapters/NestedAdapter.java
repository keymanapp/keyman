package com.keyman.engine.data.adapters;

import android.content.Context;
import android.database.DataSetObserver;
import android.util.Log;
import android.widget.ArrayAdapter;

import androidx.annotation.NonNull;

import com.keyman.engine.util.KMLog;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * An ArrayAdapter subclass that uses another ArrayAdapter (or subclass) as its data source,
 * propagating relevant changes across both to maintain a 'linked'/'nested' state.
 * Also permits use of an AdapterFilter to allow using a filtered view of that source.
 * @param <Element> The shared item type of the two Adapters.
 * @param <A> The type specification of the Adapter to be nested/linked.
 */
public class NestedAdapter<Element, A extends ArrayAdapter<Element> & ListBacked<Element>, FilterArg> extends ArrayAdapter<Element> implements ListBacked<Element>  {
  private final static String TAG = "NestedAdapter";
  final A wrappedAdapter;
  final AdapterFilter<Element, A, FilterArg> filter;
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
  static class WrapperObserver<E, S extends ArrayAdapter<E> & ListBacked<E>, A extends NestedAdapter<E, S, F>, F> extends DataSetObserver {
    // By being static and using a WeakReference here, we avoid memory leaks that would otherwise
    // prevent our owner from being GC'd.
    private WeakReference<A> listenerRef;
    private S source;
    private final F filterArg;

    WrapperObserver(A listener, S source, F filterArg) {
      this.listenerRef = new WeakReference<>(listener);
      this.source = source;
      this.filterArg = filterArg;
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
      listener._internalClear();
      listener._internalAddAll(listener.filter.selectFrom(listener.wrappedAdapter, this.filterArg));
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

  private final WrapperObserver<Element, A, NestedAdapter<Element, A, FilterArg>, FilterArg> observer;

  public NestedAdapter(@NonNull Context context, int resource, @NonNull A adapter) {
    this(context, resource, adapter, new AdapterFilter<Element, A, FilterArg>() {
        @Override
        public List<Element> selectFrom(A adapter, FilterArg dummy) {
          // Make sure to duplicate the list so that we don't accidentally try to modify our
          // wrapped adapter's (unmodifiable) contents.
          return new ArrayList<>(adapter.asList());
        }
    }, null);
  }

  public NestedAdapter(@NonNull Context context, int resource, @NonNull A adapter, AdapterFilter<Element, A, FilterArg> filter, FilterArg filterArg) {
    // Ensure that the list we pass through is writeable.  Errors may result otherwise.
    this(context, resource, adapter, filter, filterArg, new ArrayList<>(filter.selectFrom(adapter, filterArg)));
  }

  // In order for the instance to be properly constructed, the `filteredList`
  // parameter must be mutable.  Unfortunately, there's no way to robustly
  // check for that, and it illegal to construct a modifiable version
  // before the super's constructor call b/c Java limitations.
  // So... we'll keep it locked behind 'protected' for now.
  //
  // Could probably throw an extra parameter on for an 'internal' version
  // and use the current signature as a 'helper' like with the constructor above?
  protected NestedAdapter(@NonNull Context context, int resource, @NonNull A adapter, AdapterFilter<Element, A, FilterArg> filter, FilterArg filterArg, List<Element> filteredList) {
    super(context, resource, filteredList);

    this.wrappedAdapter = adapter;
    observer = new WrapperObserver<>(this, adapter, filterArg);
    this.wrappedAdapter.registerDataSetObserver(observer);

    this.filter = filter;
    this.filteredList = filteredList;
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

  public List<Element> asList() {
    return Collections.unmodifiableList(this.filteredList);
  }

  // These methods allow bypassing of our externally-visible 'linking' versions for operations
  // triggered by the other end of the link.
  protected void _internalAddAll(@NonNull Collection<? extends Element> collection) {
    try {
      super.addAll(collection);
    } catch (UnsupportedOperationException e) {
      KMLog.LogException(TAG, "_internalAddAll exception ", e);
    }
  }

  protected void _internalClear() {
    try {
      super.clear();
    } catch (UnsupportedOperationException e) {
      KMLog.LogException(TAG, "_internalClear exception ", e);
    }
  }

  // TODO:  As needed, override mutation functions so that we can reflect the changes onto
  //        the wrappedAdapter instance.  We should handle these cases directly rather than
  //        through self-triggered events, as we may only hold a subset due to use of filters.
}
