'use strict';
// Options are a JSON array, never a shell command or string split on spaces.
function browserArgs(raw) {
 if (raw === undefined || raw === '') return [];
 let parsed;
 try { parsed=JSON.parse(raw); } catch (_) { throw new Error('PK_BROWSER_ARGS must be a JSON array of argument strings'); }
 if (!Array.isArray(parsed) || parsed.length>32 || parsed.some(x => typeof x !== 'string' || x.length===0 || x.length>1024 || x.includes('\0'))) {
  throw new Error('PK_BROWSER_ARGS must be a JSON array of up to 32 nonempty argument strings');
 }
 return parsed;
}
function browserLaunchOptions(env) {
 return {headless:true,args:browserArgs(env.PK_BROWSER_ARGS),...(env.PK_BROWSER_EXECUTABLE ? {executablePath:env.PK_BROWSER_EXECUTABLE} : {})};
}
module.exports={browserArgs,browserLaunchOptions};
