using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Content;
using Microsoft.Xna.Framework.Graphics;

namespace AoC.Year2025.Day8;

public class GameObject(string modelName)
{
	protected Model Model { get; private set; }

	public void LoadContent(ContentManager contentManager)
	{
		Model = contentManager.Load<Model>(modelName);
	}

	protected void Draw(Matrix world, Matrix view, Matrix projection)
	{
		foreach (var mesh in Model.Meshes)
		{
			foreach (var effect in mesh.Effects.Cast<BasicEffect>())
			{
				effect.World = world;
				effect.View = view;
				effect.Projection = projection;

				effect.EnableDefaultLighting();
				effect.PreferPerPixelLighting = true;
				ApplyEffect(effect);
			}
			mesh.Draw();
		}
	}

	protected virtual void ApplyEffect(BasicEffect effect) { }
}
