using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test;

[TestClass]
public class HashTest
{
	[TestMethod]
	public void MD5_Once()
	{
		Assert.AreEqual("577571be4de9dcce85a041ba0410f29f", Hash.MD5("abc0"));
	}

	[TestMethod]
	public void MD5_Multiple()
	{
		Assert.AreEqual("a107ff634856bb300138cac6568c0f24", Hash.MD5("abc0", 2017));
	}
}
