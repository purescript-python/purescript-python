using Diana;
using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

namespace Impurescript
{
    using NameSpace = Dictionary<string, DObj>;

    public static class ImpurescriptExts
    {

        public static void SetValue(this NameSpace self, string v, DObj o)
        {
            self[v] = o;
        }
        public static void SetValue(this DModule self, string v, DObj o)
        {
            self.fields[v] = o;
        }

        public static DObj GetValue(this DModule self, string v)
        {
            return self.fields.TryGetValue(v, out var o) ? o : DNone.unique;
        }
        public static DObj GetValue(this NameSpace self, string v)
        {
            return self.TryGetValue(v, out var o) ? o : DNone.unique;
        }

    }


    [Serializable]
    public class Impurescript
    {
        public string ApplicationPath;

        public Impurescript(string path = null)
        {
            ApplicationPath = path ?? Environment.CurrentDirectory;
            ModuleCaches = new Dictionary<string, DModule>();
            main_calls = new List<Action>();
        }
        static List<Action> main_calls;

        public Dictionary<string, DModule> ModuleCaches;

        static DObj poly_div(DObj l, DObj r)
        {
            var i = l as DInt;
            if (i == null)
            {
                return l.__truediv__(r);
            }
            return l.__floordiv__(r);
        }

        static InternString tag = "tag".toIntern();
        static DObj js_new(DObj[] args)
        {
            if(args.Length < 1)
                throw new ArgumentException("cannot new with zero arguments.");
            var f = args[0];
            var obj = new Dictionary<DObj, DObj>();
            obj[tag] = f;
            args[0] = MK.Dict(obj);
            return f.__call__(args);
        }

        DModule ExecFromPath(string appPath, string path)
        {
            var apis = new DianaScriptAPIs();
            var globals = apis.InitGlobals();
            var mod = new DModule(appPath);
            var exported = mod.fields;
            mod.fields = globals;
            SetupNameSpace(mod, appPath);

            var ast = DianaScriptAPIs.Parse(path);
            var exec = DianaScriptAPIs.compileModule(ast, path);
            exec(globals);

            var main = globals.GetValue("x_main");
            if (main is DStaticFunc f)
            {
                
                main_calls.Add(() => f.__call__(DNone.unique));
            }

            var exports = globals.GetValue("exports");

            mod.fields = exported;
            if (exports is DDict dict)
            {
                foreach (var kv in dict.dict)
                {
                    mod.fields[(string)(DString)kv.Key] = kv.Value;
                }
            }
            return mod;
        }

        public static List<DFunc> funcs = new List<DFunc>
        {
            MK.Func1("idiv", x => MK.Func1("idiv", y => MK.Int(((DInt) x).value / ((DInt) y).value ))),
            MK.Func1("fdiv", x => MK.Func1("fdiv", y => x.__truediv__(y))),
        };
        void SetupNameSpace(DModule mod, string appPath)
        {
            mod.SetValue("new", MK.FuncN("new", js_new));
            mod.SetValue("div", MK.Func2("div", poly_div));
            mod.SetValue("__path__", MK.String(appPath));
            mod.SetValue("module", mod);
            mod.SetValue("require", MK.Func1("require", x => Require(mod, (string)(DString)x)));

            foreach(var func in funcs)
            {
                mod.SetValue("_" + func.name, func);
            }
            
        }

        string AbsRelativePath(string relativeToAbs, string absPath)
        {
            return Path.GetFullPath(absPath, Path.GetDirectoryName(relativeToAbs));
        }



        string resolveAbsPathFromCurrent(DModule mod, string relPath)
        {
            string currentAppPath = (string)(DString)mod.GetValue("__path__");
            var currentAbsPath = Path.GetFullPath(currentAppPath, ApplicationPath);
            var absPath = AbsRelativePath(currentAbsPath, relPath);
            return absPath;
        }
        string getAppPath(string absPath)
        {
            return Path.GetRelativePath(ApplicationPath, absPath);
        }

        DModule ExecutePathWithNewModule(string absPath)
        {
            var appPath = getAppPath(absPath);
            if (ModuleCaches.TryGetValue(appPath, out var value))
                return value;
            var content = File.ReadAllText(absPath);
            return ExecFromPath(appPath, absPath);
        }

        public void ExecutePath(string relPath)
        {
            var absPath = Path.GetFullPath(relPath);
            ExecutePathWithNewModule(absPath);
        }

        DModule Require(DModule oldEngine, string relPath)
        {
            var absPath = resolveAbsPathFromCurrent(oldEngine, relPath);
            return ExecutePathWithNewModule(absPath);
        }


#if CONSOLE
        public static void Main(string[] args)
        {
            var imps = new Impurescript();
            args.ToList().ForEach(imps.ExecutePath);
            main_calls.ForEach(runmain => runmain());
        }
#endif
    }
}