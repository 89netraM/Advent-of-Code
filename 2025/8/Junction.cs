using Microsoft.Xna.Framework;

namespace AoC.Year2025.Day8;

public class Junction(Vector3 position) : GameObject("present-a-cube")
{
	public void Draw(Matrix view, Matrix projection)
	{
		Draw(Matrix.CreateTranslation(position), view, projection);
	}
}
