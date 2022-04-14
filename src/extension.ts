import * as vscode from 'vscode';
import * as path from 'path';

import { InteractiveProcessHandle } from './repljs';
import { injectFileName, showProgress } from './utils';
import { Visual, CommandType } from './Visual';

type Repl = InteractiveProcessHandle;

export function activate(context: vscode.ExtensionContext) {
  let _ghciInstance: InteractiveProcessHandle;
  let _activeCwd = '';

  const visuals: { [document: string]: { [identifier: string]: Visual } } = {};

  const getGhci = async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) throw "No editor is active.";

    const filename = editor.document.fileName;
    let dir = path.parse(filename).dir;
    // Fix for Windows uppercase requirement for drive letters
    if (dir[1] === ':') dir = dir.replace(dir[0], dir[0].toUpperCase());
    const cwd = path.join(context.extensionPath, 'interactive-map').replace(/\\/g, "/");

    // console.log(_ghciInstance, cwd, _activeCwd);
    if (!_ghciInstance || cwd !== _activeCwd) {
      const cmd = `cabal repl Main --ghc-options=-i${dir}`.replace(/\\/g, "/");
      console.log(cmd);
      _ghciInstance = new InteractiveProcessHandle(cmd, [], { cwd });
      await _ghciInstance.call('');
      _activeCwd = cwd;
    }
    return _ghciInstance;
  };

  const commandFunc = async (commandType: CommandType) => {
    if (!vscode.window.activeTextEditor) {
      return;
    }
    const ghciInstancePromise = getGhci();
    const editor = vscode.window.activeTextEditor;
    const document = editor.document;
    
    const documentId = document.uri.toString(true);
    if (!(documentId in visuals)) visuals[documentId] = {};

    const progressNotification = showProgress();

    const filename = editor.document.fileName;
    let { dir } = path.parse(filename);
    if (dir[1] === ':') dir = dir.replace(dir[0], dir[0].toUpperCase());

    await injectFileName(context);

    const wordRange = editor.document.getWordRangeAtPosition(editor.selection.start);
    const identifier = editor.document.getText(wordRange);
    // console.log("Identifier: ", identifier);

    const line = editor.selection.active.line;
    const visual = await Visual.newVisual(context, ghciInstancePromise, identifier, line, commandType);

    visuals[documentId][identifier] = visual;
    progressNotification.end();
  };

  context.subscriptions.push(
    vscode.workspace.onDidSaveTextDocument(async (document: vscode.TextDocument) => {
      // console.log("refreshing");
      // console.log(visuals);
      const documentId = document.uri.toString(true);
      // console.log(documentId)
      if (documentId in visuals) {
        for (const identifier in visuals[documentId]) {
          console.info("Refreshing: ", identifier);
          await visuals[documentId][identifier].refreshHtml();
        }
      }
    }),
    vscode.commands.registerCommand('visualise.identifier', () => commandFunc(CommandType.Variable)),
    vscode.commands.registerCommand('visualise.function', () => commandFunc(CommandType.Pattern)),
    vscode.commands.registerCommand('visualise.randomfunction', () => commandFunc(CommandType.RandomPattern)),
  );
  console.log("registered");
}
