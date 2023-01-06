/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.keyman.engine;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.LinearGradient;
import android.graphics.Paint;
import android.graphics.Paint.Style;
import android.graphics.Path;
import android.graphics.RectF;
import android.graphics.Shader;
import android.util.AttributeSet;
import android.util.DisplayMetrics;
import android.view.View;
import android.view.WindowManager;

final class KMKeyPreviewView extends View {

  private float density;
  private float viewWidth, viewHeight;
  private Paint fillPaint, borderPaint;
  private int bgColor, bgColor2, borderColor;
  private float strokeWidth;
  private float borderRadius;
  private Path path;
  private RectF rect;

  public KMKeyPreviewView(Context context, AttributeSet attrs) {
    super(context, attrs);
    WindowManager wm = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
    DisplayMetrics metrics = new DisplayMetrics();
    wm.getDefaultDisplay().getMetrics(metrics);
    density = metrics.density;

    viewWidth = metrics.widthPixels;
    viewHeight = metrics.heightPixels;
    borderRadius = 12 * density;
    strokeWidth = 4.0f;
    bgColor = context.getResources().getColor(R.color.key_preview_bg);
    bgColor2 = context.getResources().getColor(R.color.key_preview_bg2);
    borderColor = context.getResources().getColor(R.color.key_preview_border);

    fillPaint = new Paint();
    fillPaint.setStyle(Style.FILL);
    fillPaint.setAlpha(250);
    fillPaint.setAntiAlias(true);

    borderPaint = new Paint();
    borderPaint.setStyle(Style.STROKE);
    borderPaint.setStrokeWidth(strokeWidth);
    borderPaint.setAntiAlias(true);

    initDraw();
  }

  @Override
  protected void onDraw(Canvas canvas) {
    canvas.drawPath(path, fillPaint);
    canvas.drawPath(path, borderPaint);
  }

  public void redraw() {
    initDraw();
    this.invalidate();
  }

  public RectF setKeySize(int width, int height) {
    RectF viewFrame = new RectF();
    viewWidth = width * 1.6f;
    viewHeight = height * 1.6f;
    viewFrame.set(0, 0, viewWidth, viewHeight);
    return viewFrame;
  }

  public void setBackgroundColor(int color) {
    bgColor = color;
  }

  public void setBackgroundColor2(int color) {
    bgColor2 = color;
  }

  public void setBorderColor(int color) {
    borderColor = color;
  }

  private void initDraw() {
    float midX = viewWidth / 2.0f;
    float midY = viewHeight - viewHeight / 4.0f;
    float viewRight = viewWidth - strokeWidth;
    float viewBottom = viewHeight - strokeWidth;
    float r = borderRadius - strokeWidth;

    fillPaint.setShader(new LinearGradient(0, midY - strokeWidth, 0, viewBottom, bgColor, bgColor2, Shader.TileMode.CLAMP));
    borderPaint.setColor(borderColor);

    path = new Path();
    path.moveTo(viewWidth / 2.0f, strokeWidth);
    rect = new RectF(strokeWidth, strokeWidth, strokeWidth + r, strokeWidth + r);
    path.arcTo(rect, 270, -90);
    path.lineTo(strokeWidth, midY);
    path.lineTo(midX, viewBottom);
    path.lineTo(viewRight, midY);
    rect.set(viewRight - r, strokeWidth, viewRight, strokeWidth + r);
    path.arcTo(rect, 0, -90);
    path.close();
  }
}