import * as child_process from 'child_process';
export class InteractiveProcessHandle {
  private process: any;

  public outputLog: string;
  public latestOutput: string;

  private replPrompt = "###DONE###";

  private processToPromise() {
    return new Promise<string>((resolve, reject) => {
      this.process.stdout.removeAllListeners();
      this.process.stderr.removeAllListeners();
      this.process.stdin.removeAllListeners();

      let lastString = '';

      this.process.stdout.on("data", (data: any) => {
        data = data.toString().trim();
        // console.log('Data: ', data);
        // console.log('LS: ', lastString);
        lastString += data;
        if (lastString.endsWith(this.replPrompt)) {
          // console.log('Response: ', lastString);
          resolve(lastString.replace(this.replPrompt,''));
          lastString = ''; // Reset lastString for next call
        }
      });
      this.process.stderr.on("data", (data: any) => {
        data = data.toString().trim();
        console.warn(data);
      });
      this.process.stdin.on("error", () => {
        console.log("Failure in stdin! ... error");
        reject();
      });
      this.process.stdin.on("close", () => {
        console.log("Failure in stdin! ... close");
        reject();
      });
      this.process.stdin.on("end", () => {
        console.log("Failure in stdin! ... end");
        reject();
      });
      this.process.stdin.on("disconnect", () => {
        console.log("Failure in stdin! ... disconnect");
        reject();
      });
      this.process.stdout.on("error", () => {
        console.log("Failure in stdout! ... error");
        reject();
      });
      this.process.stdout.on("close", () => {
        console.log("Failure in stdout! ... close");
        reject();
      });
      this.process.stdout.on("end", () => {
        console.log("Failure in stdout! ... end");
        reject();
      });
      this.process.stderr.on("error", () => {
        console.log("Failure in stderr! ... error");
        reject();
      });
      this.process.stderr.on("close", () => {
        console.log("Failure in stderr! ... close");
        reject();
      });
      this.process.stderr.on("end", () => {
        console.log("Failure in stderr! ... end");
        reject();
      });
    });
  }

  public call(command: string): Promise<string> {
    console.log(`called: "${command.trim()}"`);
    const promise = this.processToPromise();
    this.process.stdin.write(command+'\n');
    return promise;
  }

  constructor(call: any, args: any, options: any) {
    this.latestOutput = "";
    this.outputLog = "";

    this.process = child_process.spawn(call, args, { shell: true, ...options });
    this.process.stdout.setEncoding('utf8');
    this.process.stderr.setEncoding('utf8');
  }
}
