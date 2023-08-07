import { spawn } from 'child_process';

export const spawnChild = async function (cmd: string, params: string[]) {
  const child = spawn(cmd, params);

  let data = "";
  for await (const chunk of child.stdout) {
    data += chunk;
  }

  let error = "";
  for await (const chunk of child.stderr) {
    error += chunk;
  }

  const exitCode = await new Promise((resolve) => {
    child.on('close', resolve);
  });

  if (exitCode) {
    throw new Error(`subprocess error exit ${exitCode}, ${error}`);
  }

  return data;
}
