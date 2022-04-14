import * as vscode from 'vscode';

import { InteractiveProcessHandle } from './repljs';
import { showProgress } from './utils';

type Repl = InteractiveProcessHandle;
export enum CommandType { Variable, Pattern, RandomPattern }

interface Message {
  refresh?: boolean,
  key: string,
  value: string,
  opType: 'Create' | 'Update' | 'Delete',
}

export class Visual {
  ghciPromise: Promise<Repl>;
  identifier: string;
  line: number;
  commandType: CommandType;
  inset: vscode.WebviewEditorInset;

  constructor(context: vscode.ExtensionContext, ghciPromise: Promise<Repl>, identifier: string, line: number, commandType: CommandType) {
    if (!vscode.window.activeTextEditor) throw "No editor is active.";

    this.ghciPromise = ghciPromise;
    this.identifier = identifier;
    this.line = line;
    this.commandType = commandType;

    this.inset = vscode.window.createWebviewTextEditorInset(
      vscode.window.activeTextEditor, line-1, 9,
      { localResourceRoots: [ vscode.Uri.file(context.extensionPath) ], enableScripts: true, }
    );
    this.inset.webview.html = loadingPage;
    this.inset.webview.onDidReceiveMessage(
        async (message: Message) => {
          console.log(message);
          if (!vscode.window.activeTextEditor) {
            throw "No editor is active.";
          }
          const progressNotification = showProgress();
          
          if (message.refresh) {
            await this.refreshHtml();
            progressNotification.end();
            return;
          }
          await this.crudAction(message);
          progressNotification.end();
          return;
        },
        undefined,
        context.subscriptions
      );
      this.inset.onDidDispose(() => {
        console.log('WEBVIEW disposed...:(');
      });
  }

  static async newVisual(context: vscode.ExtensionContext, ghciPromise: Promise<Repl>, identifier: string, line: number, commandType: CommandType) {
    const a = new Visual(context, ghciPromise, identifier, line, commandType);
    await a.refreshHtml();
    return a;
  }

  async refreshHtml(retry = true) {
    const ghciInstance = await this.ghciPromise;
    const load = await ghciInstance.call(':l Main');
    console.log("load: ", load);
    const command = ['graph', 'pattern', 'randomPattern'][this.commandType];
    const response = await ghciInstance.call(`${command} File.${this.identifier} ${this.commandType == CommandType.Pattern ? 'sampleList' : ''}`);
    // console.log("graph: " + response);
    try{
      const parsed = JSON.parse(response);
      // console.debug(parsed);
      this.inset.webview.html = parsed.html;
    }catch(e){
      console.error(e);
      console.error(response);
      retry && this.refreshHtml(false);
    }
  }

  async crudAction(message: Message) {
    if (!vscode.window.activeTextEditor) throw "No editor is active.";

    const { key, value, opType } = message;
          
    const exp = opType == 'Delete' ? 'Nothing' : `Just ${value}`;

    const ghciInstance = await this.ghciPromise;
    await ghciInstance.call(':l Main');
    const response = await ghciInstance.call(`edit (${opType}) [(${key})] (${exp}) (File.${this.identifier}) `);
    const parsed = JSON.parse(response);
    // console.info(parsed);
    const startposition = new vscode.Position(this.line,0);
    const endingposition = new vscode.Position(this.line+1,0);
    const range = new vscode.Range(startposition,endingposition);
    vscode.window.activeTextEditor.edit(editBuilder => {
      editBuilder.replace(range, `${this.identifier} = ${parsed.code}\n`);
    });
    await vscode.window.activeTextEditor.document.save();

    // Don't refresh to avoid weird race condition. Autosave will refresh anyway
    // await this.refreshHtml();
  }
}


const loadingPage = '\
  <html><head>\
  <style>\
  .loader {\
  border: 16px solid #f3f3f3; /* Light grey */\
  border-top: 16px solid #3498db; /* Blue */\
  border-radius: 50%;\
  width: 120px;\
  height: 120px;\
  animation: spin 2s linear infinite;\
  }\
  \
  @keyframes spin {\
  0% { transform: rotate(0deg); }\
  100% { transform: rotate(360deg); }\
  }\
  </style>\
  <body><div class="loader"></div>\
';