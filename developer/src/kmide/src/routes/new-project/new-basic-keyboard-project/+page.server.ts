import { BasicKeyboardProject } from "./basic-keyboard-project.svelte";

import { error, redirect } from '@sveltejs/kit';
import type { PageServerLoad } from './$types';
import { BasicKeyboardProjectValidation } from "./validation";
import { basicKeyboardProjects } from "./server-data";

export const load: PageServerLoad = async ({ cookies }) => {
  const token = cookies.get('token');
  if(!token) {
    error(401, 'Unauthorized: load: Invalid token');
  }

  return {
    project: basicKeyboardProjects[token] ?? { ... new BasicKeyboardProject() },
    isHostedInTike: !!cookies.get('tike')
  }
};

export const actions = {
	default: async ({ cookies, request }) => {
    const token = cookies.get('token');
    if(!token) {
      error(401, 'Unauthorized: default: Invalid token');
    }

		const data = await request.formData();
    const project: any = {};
    for(const pair of data.entries()) {
      project[pair[0]] = pair[1];
    }

    const validation = new BasicKeyboardProjectValidation(project, {fields:[]});
    if(!validation.validateAll()) {
      error(403, validation.summarize());
    }

    basicKeyboardProjects[token] = project;

    redirect(303, cookies.get('tike')
      ? 'http://keyman/ok'
      : '/new-project/new-basic-keyboard-project/result'
    );
	}
};
