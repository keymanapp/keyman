<script lang="ts">
  import type { Validation } from "$lib/validation.svelte";
  import { Input, type InputType } from "@sveltestrap/sveltestrap";
  let {
    validation,
    value = $bindable(),
    validationState = $bindable(),
    name,
    placeholder = "",
    type = "text",
    oninput = undefined,
  }: {
    validation: Validation,
    value: any,
    validationState: any,
    name: string,
    placeholder?: string,
    type?: InputType,
    oninput?: () => void,
  } = $props();

  const field = $state(validationState.fields.find( (f:any) => f.name == name));

  function validate() {
    validation.validate(name, value, false);
  }

  function handleInput() {
    field.hasUserModified = true;
    if(oninput) oninput();
    return validate();
  }

  function handleFocus() {
    field.hasFocused = true;
  }

  function handleBlur() {
    field.hasBlurred = true;
    return validate();
  }
</script>

<Input
  id={name}
  {type}
  {name}
  feedback={field.feedback}
  valid={field.isValid}
  invalid={field.isInvalid}
  {placeholder}
  bind:value={value}
  on:input={handleInput}
  on:focus={handleFocus}
  on:blur={handleBlur}
/>
