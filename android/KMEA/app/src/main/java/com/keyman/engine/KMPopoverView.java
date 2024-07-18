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
import android.graphics.Point;
import android.graphics.RectF;
import android.graphics.Shader;
import android.util.AttributeSet;
import android.util.DisplayMetrics;
import android.view.View;
import android.view.WindowManager;

final class KMPopoverView extends View {

  private float density;
  private float viewWidth, viewHeight;
  private Paint fillPaint, borderPaint;
  private int bgColor, bgColor2, borderColor;
  private float strokeWidth;
  private float borderRadius;
  private Path path;
  private RectF rect;
  private float arrowWidth, arrowHeight, arrowPosX, arrowPosY;

  public KMPopoverView(Context context, AttributeSet attrs) {
    super(context, attrs);
    Point size = KMManager.getWindowSize(context);
    density = KMManager.getWindowDensity(context);

    viewWidth = size.x;
    viewHeight = size.y;
    borderRadius = 6 * density;
    strokeWidth = 2.0f;
    bgColor = context.getResources().getColor(R.color.popup_bg);
    bgColor2 = context.getResources().getColor(R.color.popup_bg2);
    borderColor = context.getResources().getColor(R.color.popup_border);
    arrowWidth = 21 * density;
    arrowHeight = 8 * density;
    arrowPosX = viewWidth / 2;

    fillPaint = new Paint();
    fillPaint.setStyle(Style.FILL);
    fillPaint.setAlpha(240);
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

  public void setSize(int width, int height) {
    viewWidth = width;
    viewHeight = height;
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

  public void setArrowSize(float width, float height) {
    arrowWidth = width;
    arrowHeight = height;
  }

  public void setArrowPosX(float x) {
    if (x < (arrowWidth / 2.0f + borderRadius + strokeWidth)) {
      arrowPosX = arrowWidth / 2.0f + borderRadius + strokeWidth;
    } else if (x > (viewWidth - arrowWidth / 2.0f - borderRadius - strokeWidth)) {
      arrowPosX = viewWidth - arrowWidth / 2.0f - borderRadius - strokeWidth;
    } else {
      arrowPosX = x;
    }
  }

  private void initDraw() {
    arrowPosY = viewHeight - (arrowHeight + strokeWidth);

    fillPaint.setShader(new LinearGradient(0, 0, 0, viewHeight, bgColor, bgColor2, Shader.TileMode.CLAMP));
    borderPaint.setColor(borderColor);

    path = new Path();
    path.moveTo(borderRadius + strokeWidth, arrowPosY);
    path.lineTo(arrowPosX - arrowWidth / 2.0f, arrowPosY);
    path.lineTo(arrowPosX, viewHeight - strokeWidth);
    path.lineTo(arrowPosX + arrowWidth / 2.0f, arrowPosY);
    rect = new RectF(viewWidth - (borderRadius + strokeWidth), arrowPosY - borderRadius, viewWidth - strokeWidth, arrowPosY);
    path.arcTo(rect, 90, -90);
    rect.set(viewWidth - (borderRadius + strokeWidth), strokeWidth, viewWidth - strokeWidth, borderRadius + strokeWidth);
    path.arcTo(rect, 360, -90);
    rect.set(strokeWidth, strokeWidth, borderRadius + strokeWidth, borderRadius + strokeWidth);
    path.arcTo(rect, 270, -90);
    rect.set(strokeWidth, arrowPosY - borderRadius, borderRadius + strokeWidth, arrowPosY);
    path.arcTo(rect, 180, -90);
    path.close();
  }
}