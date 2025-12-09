#if GAME
using System;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Input;
using AocVector3 = AoC.Library.Vector3;
using Vector3 = Microsoft.Xna.Framework.Vector3;

namespace AoC.Year2025.Day8;

public class Day8Game : Game
{
	private static readonly TimeSpan ClickTime = TimeSpan.FromMilliseconds(250);

	private readonly GraphicsDeviceManager graphics;
	private readonly Camera camera;

	private readonly IImmutableList<Junction> junctions;
	private readonly IImmutableList<LightString> lightStrings;
	private int attachedLightStrings = 0;

	private (TimeSpan, Point, ButtonState)? previousMouseState = null;

	public Day8Game(
		IImmutableList<AocVector3> junctions,
		IImmutableList<(AocVector3, AocVector3)> pairs
	)
	{
		graphics = new(this);
		camera = new(junctions);
		this.junctions = junctions.Select(v => new Junction(ToVector3(v))).ToImmutableArray();
		lightStrings = pairs
			.Select(pair => new LightString(ToVector3(pair.Item1), ToVector3(pair.Item2)))
			.ToImmutableArray();

		Window.Title = "Advent of Code 2025 Day 8";
		Window.AllowUserResizing = true;
		IsMouseVisible = true;
	}

	protected override void LoadContent()
	{
		Content.RootDirectory = "Content";
		foreach (var junction in junctions)
		{
			junction.LoadContent(Content);
		}
		foreach (var lightString in lightStrings)
		{
			lightString.LoadContent(Content);
		}
	}

	protected override void Update(GameTime gameTime)
	{
		var mouseState = Mouse.GetState(Window);
		camera.Update(mouseState, graphics.GraphicsDevice.Viewport.AspectRatio);

		if (IsMouseClick(gameTime, mouseState))
		{
			if (attachedLightStrings < lightStrings.Count)
			{
				lightStrings[attachedLightStrings++].IsAttached = true;
			}
		}

		base.Update(gameTime);
	}

	protected override void Draw(GameTime gameTime)
	{
		graphics.GraphicsDevice.Clear(Color.Black);

		foreach (var junction in junctions)
		{
			junction.Draw(camera.ViewMatrix, camera.ProjectionMatrix);
		}
		foreach (var lightString in lightStrings)
		{
			lightString.Draw(camera.ViewMatrix, camera.ProjectionMatrix);
		}

		base.Draw(gameTime);
	}

	private bool IsMouseClick(GameTime gameTime, MouseState mouseState)
	{
		if (previousMouseState is not var (previousTime, previousPoint, previousButton))
		{
			previousMouseState = (gameTime.TotalGameTime, mouseState.Position, mouseState.LeftButton);
			return false;
		}

		if (previousButton == mouseState.LeftButton)
		{
			return false;
		}
		previousMouseState = (gameTime.TotalGameTime, mouseState.Position, mouseState.LeftButton);

		return previousButton is ButtonState.Pressed
			&& mouseState.LeftButton is ButtonState.Released
			&& previousPoint == mouseState.Position
			&& gameTime.TotalGameTime < previousTime + ClickTime;
	}

	public static Vector3 ToVector3(AocVector3 aocVector3) => new(aocVector3.X, aocVector3.Y, aocVector3.Z);
}
#endif
