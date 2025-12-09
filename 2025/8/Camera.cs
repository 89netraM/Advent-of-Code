// using System.Collections.Immutable;
// using System.Linq;
// using AoC.Library;
// using Microsoft.Xna.Framework;
// using Microsoft.Xna.Framework.Input;
// using AocVector3 = AoC.Library.Vector3;
// using Vector3 = Microsoft.Xna.Framework.Vector3;

// namespace AoC.Year2025.Day8;

// public class Camera
// {
// 	private const float DistanceSpeed = -1.0f;
// 	private const float YawSpeed = -0.01f / float.Pi;
// 	private const float PitchSpeed = -0.01f / float.Pi;
// 	private const float PitchRange = float.Pi * 0.45f;
// 	public float Distance { get; set; } = 1000.0f;
// 	public float Yaw { get; set; } = float.Pi / 4.0f;
// 	public float Pitch { get; set; } = 0.0f;

// 	public Matrix ViewMatrix { get; private set; } = Matrix.Identity;
// 	public Matrix ProjectionMatrix { get; private set; } = Matrix.Identity;

// 	private readonly Vector3 center;

// 	private int? prevScrollWheelValue = null;
// 	private Point? prevMousePosition = null;

// 	public Camera(IImmutableList<AocVector3> junctions)
// 	{
// 		var aocCenter = junctions
// 			.Aggregate(AocVector3.Zero, (a, b) => a + b)
// 			.Let(acc => acc / junctions.Count);
// 		center = Day8Game.ToVector3(aocCenter);
// 		Distance = (float)junctions.Max(j => aocCenter.Distance(j)) * 1.5f;
// 	}

// 	public void Update(MouseState mouseState, float aspectRatio)
// 	{
// 		Distance = float.Clamp(Distance + GetScrollWheelDelta(mouseState) * DistanceSpeed, 50.0f, 200000.0f);
// 		var mousePositionDelta = GetMousePositionDelta(mouseState);
// 		Yaw += mousePositionDelta.X * YawSpeed;
// 		Pitch = float.Clamp(Pitch + mousePositionDelta.Y * PitchSpeed, -PitchRange, PitchRange);

// 		var position =
// 			center
// 			+ Vector3.Transform(Vector3.Backward, Quaternion.CreateFromYawPitchRoll(Yaw, Pitch, 0.0f)) * Distance;

// 		ViewMatrix = Matrix.CreateLookAt(position, center, Vector3.Up);
// 		ProjectionMatrix = Matrix.CreatePerspectiveFieldOfView(
// 			MathHelper.ToRadians(90.0f),
// 			aspectRatio,
// 			0.01f,
// 			210000.0f
// 		);
// 	}

// 	private int GetScrollWheelDelta(MouseState mouseState)
// 	{
// 		var pswv = prevScrollWheelValue;
// 		prevScrollWheelValue = mouseState.ScrollWheelValue;
// 		return pswv is int p ? mouseState.ScrollWheelValue - p : 0;
// 	}

// 	private Point GetMousePositionDelta(MouseState mouseState)
// 	{
// 		var pmp = prevMousePosition;
// 		prevMousePosition = mouseState.Position;
// 		return mouseState.LeftButton is ButtonState.Pressed && pmp is Point p ? mouseState.Position - p : new(0, 0);
// 	}
// }
