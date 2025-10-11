import { json } from '@sveltejs/kit';
import { basicKeyboardProjects } from '../server-data';

export async function GET({cookies}) {
  const token = cookies.get('token');
  if(!token) {
    return json({result: 'token parameter is required'}, {status: 401, statusText: 'token parameter is required'});
  }
  const project = basicKeyboardProjects[token];
  if(!project) {
    return json({result: 'project not found for current user'}, {status: 404, statusText: 'project not found for current user'});
  }
  return json(project);
}