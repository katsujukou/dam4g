export const loadCompilerImpl = async () => {
  const module = await import("/assets/compiler.min.js");
  console.log(module)
  return module;
}

export const compileImpl = function (compiler, src) {
  const res = compiler.compileJs(src);
  return res;
}