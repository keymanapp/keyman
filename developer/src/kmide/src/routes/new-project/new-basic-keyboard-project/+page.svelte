<script lang="ts">
  import { Container, Row, Form, Input, Button, Alert, InputGroup, Label, } from "@sveltestrap/sveltestrap";
  import { BasicKeyboardProjectValidation, type BasicKeyboardProjectValidationFields } from "./validation.js";
  import { page } from "$app/state";
  import { goto } from "$app/navigation";
  import { BasicKeyboardProject } from "./basic-keyboard-project.svelte.js";
  import ValidatedInput from "$lib/components/validated-input.svelte";
  import { cleanKeyboardID } from "./clean-keyboard-id.js";

  const isHostedInTike = $derived(page.data.isHostedInTike);

  const PATH_DELIMITER = '/'; // TODO...

  if(page.data.isHostedInTike) {
    // Hook to built-in Path Browse dialog
    (<any>globalThis)._keymanInterface = {
      setPath: function(path: string) {
        project.basePath = path;
      }
    };
  }

  let validityAlertIsOpen = $state(false);
  const project = $state(new BasicKeyboardProject());
  const validationState = $state({fields:[]} as BasicKeyboardProjectValidationFields);
  const validation = new BasicKeyboardProjectValidation(project, validationState);
  const projectFilename = $derived(includeTrailingPathDelimiter(project.basePath ?? '') + project.keyboardID + PATH_DELIMITER + project.keyboardID + '.kpj');

  function formSubmit(event: SubmitEvent) {
    validityAlertIsOpen = false;
    if(!validation.validateAll()) {
      event.preventDefault();
      validityAlertIsOpen = true;
    }
  }

  /**
   * Browse for basePath - only works when hosted in Tike
   */
  function doBrowse(event: Event) {
    event.preventDefault();
    if(isHostedInTike) {
      location.href = 'http://keyman/browse?basePath=' + encodeURIComponent(page.data.project.basePath ?? '');
    } else {
      alert('Unable to open Path Browse dialog in browser mode');
    }
  }

  function doCancel(event: Event) {
    event.preventDefault();
    if(isHostedInTike) {
      location.href = 'http://keyman/cancel';
    } else {
      goto('/');
    }
  }

  function keyboardNameInput() {
    if(!validationState.fields.find(field => field.name == 'keyboardID')?.hasUserModified) {
      project.keyboardID = cleanKeyboardID(project.keyboardName ?? '');
      validation.validate('keyboardID', project.keyboardID ?? '', true);
    }
  }

  function authorInput() {
    if(!validationState.fields.find(field => field.name == 'copyright')?.hasUserModified) {
      project.copyright = 'Copyright © '+project.author;
    }
    if(!validationState.fields.find(field => field.name == 'fullCopyright')?.hasUserModified) {
      project.fullCopyright = 'Copyright © 2025 '+project.author;
    }
  }

  function includeTrailingPathDelimiter(path: string): string {
    return path.endsWith('/') || path.endsWith('\\') ? path : path + PATH_DELIMITER;
  }
</script>

<Container fluid class="bg-light">
<Container lg>
  <Form method="post" on:submit={formSubmit} action="?">
    <Row class="mb-3 bg-secondary text-white">
      <h1>New Basic Keyboard Project</h1>
    </Row>

    <Row class="mb-3">
      <Label for="keyboardName">Keyboard name</Label>
      <InputGroup>
        <ValidatedInput bind:value={project.keyboardName} {validation} {validationState}
          name="keyboardName"
          placeholder="Name of keyboard presented to end users"
          oninput={keyboardNameInput}
        />
      </InputGroup>
    </Row>

    <Row class="mb-3">
      <Label for="description">Description</Label>
      <InputGroup>
        <ValidatedInput bind:value={project.description} {validation} {validationState}
          name="description"
          type="textarea"
        />
      </InputGroup>
    </Row>

    <Row class="mb-3">
      <Label for="author">Author</Label>
      <InputGroup>
        <ValidatedInput bind:value={project.author} {validation} {validationState}
          name="author"
          placeholder="Name of author"
          oninput={authorInput}
        />
      </InputGroup>
    </Row>

    <Row class="mb-3">
      <Label for="copyright">Copyright</Label>
      <InputGroup>
        <ValidatedInput bind:value={project.copyright} {validation} {validationState}
          name="copyright"
          placeholder="Copyright message (do not include years)"
        />
      </InputGroup>
    </Row>

    <Row class="mb-3">
      <Label for="fullCopyright">Full Copyright</Label>
      <InputGroup>
        <ValidatedInput bind:value={project.fullCopyright} {validation} {validationState}
          name="fullCopyright"
          placeholder="Full copyright message, including years"
        />
      </InputGroup>
    </Row>

    <Row class="mb-3">
      <Label for="version">Version</Label>
      <InputGroup>
        <ValidatedInput bind:value={project.version} {validation} {validationState}
          name="version"
        />
       </InputGroup>
    </Row>

    <Row class="mb-3">
      <Label for="languages">Languages</Label>
      <InputGroup>
        <ValidatedInput bind:value={project.languages} {validation} {validationState}
          name="languages"
          type="textarea"
        />
      </InputGroup>
    </Row>

    <Row class="mb-3">
      <Label for="targets">Targets</Label>
      <div>
        <Input type="checkbox" label="any" />
        <Input type="checkbox" label="windows" />
        <Input type="checkbox" label="macos" />
        <Input type="checkbox" label="linux" />
      </div>
    </Row>

    <Row class="mb-3">
      <Label for="basePath">Path</Label>
      <InputGroup>
        <ValidatedInput bind:value={project.basePath} {validation} {validationState}
          name="basePath"
        />
        {#if isHostedInTike}
          <Button on:click={doBrowse}>Browse...</Button>
        {/if}
      </InputGroup>
    </Row>

    <Row class="mb-3">
      <Label for="keyboardID">Keyboard ID</Label>
      <InputGroup>
        <ValidatedInput bind:value={project.keyboardID} {validation} {validationState}
          name="keyboardID"
        />
      </InputGroup>
    </Row>

    <Row class="mb-3">
      <Label for="projectFilename">Project Filename</Label>
      <InputGroup><!-- todo -->
        <Input
          value={projectFilename}
          id="projectFilename"
          name="projectFilename"
          readonly
          class="bg-light"
        />
      </InputGroup>
    </Row>

    <Alert isOpen={validityAlertIsOpen} color="danger">
      One or more fields contain invalid data. Please correct these fields and try again.
    </Alert>

    <Button type="submit" color="primary">OK</Button>
    <Button color="secondary" on:click={doCancel}>Cancel</Button>
  </Form>
</Container>
</Container>