import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

import { InteractiveProcessHandle } from './repljs';

type Repl = InteractiveProcessHandle;

export const showProgress = () => {
  let ret: (value: void | PromiseLike<void>) => void = () => {};
  const promise = new Promise<void>(resolve => {
      ret = resolve;
  });
  vscode.window.withProgress({
    location: vscode.ProgressLocation.Notification,
    title: "Working on it! May take some time...",
    cancellable: false
  }, (progress, token) => promise);
  return { end: ret };
};

export const readFile = async (filePath: fs.PathLike): Promise<string> => {
  try {
    return fs.promises.readFile(filePath, 'utf8');
  } catch (err) {
    console.error('Error occured while reading file!', err);
  }
  return "";
};

export const replaceInFile = async (templatePath: fs.PathLike, filePath: fs.PathLike, reps: [RegExp | string, string][]) => {
  let data = await readFile(templatePath);
  for (const [s1, s2] of reps) {
    data = data.replace(s1, s2);
  }  
  console.log(data);
  try {
    return fs.promises.writeFile(filePath, data, 'utf8');
  } catch (err) {
    console.error('Error occured while writing back file!', err);
  }
};

export const injectFileName = async (context: vscode.ExtensionContext) => {
  if (!vscode.window.activeTextEditor) {
    throw "No editor is active.";
  }
  const filename = vscode.window.activeTextEditor.document.fileName;
  let { name } = path.parse(filename);
  const tempDispPath = path.join(context.extensionPath, 'interactive-map', 'templates', 'Main.template.hs');
  const injeDispPath = path.join(context.extensionPath, 'interactive-map', 'Main.hs');
  
  await replaceInFile(tempDispPath, injeDispPath, [
    ["###REPLACE WITH NAME OF MODULE###", name],
  ]);
};
