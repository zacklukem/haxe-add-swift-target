/*
 * Copyright (C)2005-2016 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

@:coreApi class Sys {
	private static var _args:swift.NativeArray<String>;
	private static var _env:haxe.ds.StringMap<String>;
	private static var _sysName:String;

    @:functionCode('Swift.print(v, appendNewLine: false)')
	public static function print( v : Dynamic ) : Void
	{
	}

    @:functionCode('Swift.print(v, appendNewLine: true)')    
	public static function println( v : Dynamic ) : Void
	{
	}

    @:functionCode('
            if (CommandLine.arguments == nil)
			    return [String]()
            else 
		        return CommandLine.arguments
            ')
	public static function args() : Array<String>
	{
        return null;
	}

    @:functionCode('
            	guard let rawValue = getenv(s) else { return nil }
                return String(utf8String: rawValue)
            ')
	public static function getEnv( s : String ) : String
	{
	    return null;
	}

    @:functionCode('
                if (s != nil && v != nil) 
                {
                    setenv(s, v, 1/*overwrite*/)
                }
                else 
                {
                    //TODO: handle error case.
                }
            ')
	public static function putEnv( s : String, v : String ) : Void
	{
        /* 
           TODO: alternative implementation, needs import
           import Foundation

if let value = ProcessInfo.processInfo.environment["key"] {
    ...
}
           */
	}

	public static function environment() : Map<String,String>
	{
		throw "not implemented, no support in Swift.";
	}

	public static function sleep( seconds : Float ) : Void
	{
		/*try
			swift.lang.Thread.sleep(cast seconds * 1000)
		catch (e:Dynamic)
			throw e;*/

	}

	public static function setTimeLocale( loc : String ) : Bool
	{
		return false;
	}

	public static function getCwd() : String
	{
		//return new swift.io.File(".").getAbsolutePath().substr(0,-1);
        return "";
	}

	public static function setCwd( s : String ) : Void
	{
		//swift offers no support for it (!)
		throw "not implemented";
	}

	public static function systemName() : String
	{
		/*if (_sysName != null) return _sysName;
		var sname = System.getProperty("os.name").toLowerCase();
		if (sname.indexOf("win") >= 0)
			return _sysName = "Windows";
		if (sname.indexOf("mac") >= 0)
			return _sysName = "Mac";
		if (sname.indexOf("nux") >= 0)
			return _sysName = "Linux";
		if (sname.indexOf("nix") >= 0)
			return _sysName = "BSD";

		return _sysName = System.getProperty("os.name");*/
        return "";
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int
	{
		/*var pb = Process.createProcessBuilder(cmd, args);
#if swift6
		pb.redirectErrorStream(true);
#else
		pb.redirectOutput(swift.lang.ProcessBuilder.ProcessBuilder_Redirect.INHERIT);
		pb.redirectError(swift.lang.ProcessBuilder.ProcessBuilder_Redirect.INHERIT);
#end
		var proc = pb.start();
#if swift6
		var reader = new swift.io.NativeInput(proc.getInputStream());
		try
		{
			while(true) {
				var ln = reader.readLine();
				Sys.println(ln);
			}
		}
		catch(e:haxe.io.Eof) {}
#end
		proc.waitFor();
		var exitCode = proc.exitValue();
		proc.destroy();
		return exitCode;*/
        return 1;
	}

	public static function exit( code : Int ) : Void
	{
		//System.exit(code);
	}

	public static function time() : Float
	{
		//return cast(System.currentTimeMillis(), Float) / 1000;
        return 0.0;
	}

	public static function cpuTime() : Float
	{
		//return cast(System.nanoTime(), Float) / 1000000000;
        return 0.0;
	}

	@:deprecated("Use programPath instead") public static function executablePath() : String
	{
		//return getCwd();
        return "";
	}

	public static function programPath() : String {
		/*return swift.Lib.toNativeType(Sys)
			.getProtectionDomain()
			.getCodeSource()
			.getLocation().toURI().getPath();*/        return "";
	}

	public static function getChar( echo : Bool ) : Int
	{
		//TODO
		return throw "Not implemented";
	}

	public static function stdin() : haxe.io.Input
	{
		/*var _in:swift.io.InputStream = Reflect.field(System, "in");
		return new swift.io.NativeInput(_in);*/
        return null;
	}

	public static function stdout() : haxe.io.Output
	{
		//return new swift.io.NativeOutput(System.out);
        return null;
	}

	public static function stderr() : haxe.io.Output
	{
		//return new swift.io.NativeOutput(System.err);
        return null;
	}
}
