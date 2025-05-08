package com.example.chickenstock.ui.components

import android.content.Context
import android.widget.TextView
import com.example.chickenstock.R
import com.github.mikephil.charting.components.MarkerView
import com.github.mikephil.charting.data.Entry
import com.github.mikephil.charting.highlight.Highlight
import com.github.mikephil.charting.utils.MPPointF
import java.text.SimpleDateFormat
import java.util.*

class ChartMarkerView(
    context: Context,
    private val dates: List<String>  // 날짜 리스트
) : MarkerView(context, R.layout.marker_view) {

    private val textView: TextView = findViewById(R.id.markerText)

    override fun refreshContent(e: Entry?, highlight: Highlight?) {
        e?.let {
            val index = e.x.toInt()
            if (index in dates.indices) {
                val date = dates[index]
                val price = String.format("%.0f", e.y)
                textView.text = "$date\n${price}원"
            }
        }
        super.refreshContent(e, highlight)
    }

    override fun getOffset(): MPPointF {
        return MPPointF(-(width / 2f), -chartView!!.height.toFloat() + 20f)
    }
} 