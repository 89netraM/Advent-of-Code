using System.Collections.Immutable;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Content;
using Microsoft.Xna.Framework.Graphics;

namespace AoC.Year2025.Day8;

public class LightString
{
	private readonly IImmutableList<Light> lights;

	public bool IsAttached { get; set; } = false;

	public bool IsLit
	{
		get => lights[0].IsLit;
		set
		{
			foreach (var light in lights)
			{
				light.IsLit = value;
			}
		}
	}

	public LightString(Vector3 from, Vector3 to)
	{
		var dir = Vector3.Normalize(to - from);
		var distance = (int)float.Floor(Vector3.Distance(from, to) / 50.0f) - 1;
		lights = Enumerable
			.Range(0, distance)
			.Select(i => new Light(from + dir * (50.0f * i + 25.0f)))
			.ToImmutableArray();
	}

	public void LoadContent(ContentManager contentManager)
	{
		foreach (var light in lights)
		{
			light.LoadContent(contentManager);
		}
	}

	public void Draw(Matrix view, Matrix projection)
	{
		if (!IsAttached)
		{
			return;
		}

		foreach (var light in lights)
		{
			light.Draw(view, projection);
		}
	}
}

public class Light(Vector3 position) : GameObject("lantern-hanging")
{
	public bool IsLit { get; set; }

	public void Draw(Matrix view, Matrix projection)
	{
		Draw(Matrix.CreateTranslation(position), view, projection);
	}

	protected override void ApplyEffect(BasicEffect effect)
	{
		effect.EmissiveColor = IsLit ? new(1, 1, 0) : Vector3.Zero;
	}
}
