import { KmpJsonFile } from "@keymanapp/common-types"

/**
 * Write a legacy kmp.inf file to a string, following the format
 * provided by kmcomp
 * @param data
 */
export class KmpInfWriter {
  private result: string[] = [];

  constructor(private data: KmpJsonFile.KmpJsonFile) {
  }

  public write(): string {
    this.result = [];

    this.saveOptions();
    this.saveStartMenu();
    this.saveInfo();
    this.saveFiles();
    this.saveKeyboards();
    return this.result.join('\r\n') + '\r\n';
  }

  private addSection(name: string): void {
    this.result.push(`[${name}]`);
  }

  private addString(name: string, value: string): void {
    this.result.push(`${name}=${value ?? ''}`);
  }

  private addBool(name: string, value: boolean): void {
    this.result.push(`${name}=${value ? 1 : 0}`);
  }

  private saveFiles(): void {
    // content files
    if(!this.data.files?.length) {
      return;
    }

    this.addSection('Files');

    for(let i = 0; i < this.data.files.length; i++) {
      const file = this.data.files[i];
      this.addString(i.toString(), `"${file.description??''}","${file.name}",${file.copyLocation??0}`);
    }
  }

  private saveStartMenu(): void {
    // start menu
    this.addSection('StartMenu');
    this.addString('Path', this.data.startMenu?.folder);
    this.addBool('Create', (this.data.startMenu?.folder ?? '') != '');
    this.addBool('AddUninstallEntry', this.data.startMenu?.addUninstallEntry);
    this.saveStartMenuEntries();
  }

  private saveStartMenuEntries() {
    // start menu items
    if(!this.data.startMenu?.items?.length) {
      return;
    }
    this.addSection('StartMenuEntries');
    let hasIcon = false, hasLocation = false;
    for(let item of this.data.startMenu.items) {
      this.addString(item.name, `"${item.filename}","${item.arguments ?? ''}"`);
      hasIcon = hasIcon || !!item.icon;
      hasLocation = hasLocation || (item.location??'psmelStartMenu') != 'psmelStartMenu';
    }

    // For backward compatibility, we need these to go in another section.
    // Earlier versions of Keyman ignored these values (as in Keyman 6?, 7?)

    if(hasIcon) {
      this.addSection('StartMenuEntries_Icon');
      for(let item of this.data.startMenu.items) {
        if(item.icon) {
          this.addString(item.name, item.icon);
        }
      }
    }

    if(hasLocation) {
      this.addSection('StartMenuEntries_Location');
      for(let item of this.data.startMenu.items) {
        if(item.location != 'psmelStartMenu') {
          this.addString(item.name, '1'); // psmelDesktop ordinal = 1 is the only other supported value
        }
      }
    }
  }

  private saveInfo() {
    // info
    if(!this.data.info?.author?.description &&
        !this.data.info?.copyright?.description &&
        !this.data.info?.name?.description &&
        !this.data.info?.version?.description &&
        !this.data.info?.website?.description) {
      return;
    }

    // NOTE: this may be written in a different order than kmcomp would because
    // the Delphi code had an arbitrary order for the info fields

    let addedSection = false;

    const saveInfoItem = (title: string, field: KmpJsonFile.KmpJsonFileInfoItem) => {
      if(!field || !field.description) return;
      if(!addedSection) {
        this.addSection('Info');
        addedSection = true;
      }
      this.addString(title, `"${field.description??''}","${field.url??''}"`);
    };

    saveInfoItem('Name', this.data.info?.name);
    saveInfoItem('Copyright', this.data.info?.copyright);
    saveInfoItem('Author', this.data.info?.author);
    saveInfoItem('Version', this.data.info?.version);
    saveInfoItem('WebSite', this.data.info?.website);
  }

  private saveOptions() {
    // options
    this.addSection('Package');
    this.addString('Version', '7.0'); // This is the only version number ever supported in kmp.inf
    this.addString('ExecuteProgram', this.data.options.executeProgram);
    if(this.data.options.readmeFile) {
      this.addString('ReadMeFile', this.data.options.readmeFile);
    }
    if(this.data.options.graphicFile) {
      this.addString('GraphicFile', this.data.options.graphicFile);
    }
  }

  private saveKeyboards() {
    // keyboards
    for(let i = 0; i < this.data.keyboards?.length ?? 0; i++) {
      const keyboard = this.data.keyboards[i];
      this.addSection('Keyboard'+i.toString());
      this.addString('Name', keyboard.name);
      this.addString('ID', keyboard.id);
      this.addString('Version', keyboard.version);
      if(keyboard.rtl) {
        this.addBool('RTL', true);
      }
      if(keyboard.oskFont) {
        this.addString('OSKFont', keyboard.oskFont);
      }
      if(keyboard.displayFont) {
        this.addString('DisplayFont', keyboard.displayFont);
      }

      for(let j = 0; j < keyboard.languages?.length ?? 0; j++) {
        const language = keyboard.languages[j];
        this.addString('Language'+j.toString(), language.id+','+language.name);
      }
    }
  }
}

