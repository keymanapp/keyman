import { redirect } from '@sveltejs/kit';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async ({ cookies }) => {
  cookies.set('token', '1234', {path:'/'});

  redirect(303, '/');
};

