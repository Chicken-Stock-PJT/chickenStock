package com.example.chickenstock.ui.components

import android.content.Context
import android.widget.TextView
import com.example.chickenstock.R
import com.github.mikephil.charting.components.MarkerView
import com.github.mikephil.charting.data.Entry
import com.github.mikephil.charting.highlight.Highlight
import com.github.mikephil.charting.utils.MPPointF

class ChartMarkerView(
    context: Context,
    private val dates: List<String>
) : MarkerView(context, R.layout.marker_view) {
    
    private val tvDate: TextView = findViewById(R.id.tvDate)
    private val tvValue: TextView = findViewById(R.id.tvValue)

    override fun refreshContent(e: Entry?, highlight: Highlight?) {
        e?.let {
            val index = it.x.toInt()
            val date = if (index >= 0 && index < dates.size) dates[index] else ""
            tvDate.text = date
            tvValue.text = String.format("%,.0f원", it.y)
        }
        super.refreshContent(e, highlight)
    }

    override fun getOffset(): MPPointF {
        return MPPointF((-(width / 2)).toFloat(), (-height).toFloat())
    }
} 