package org.hoshino9.luogu.utils

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import org.hoshino9.luogu.record.TestCase
import org.hoshino9.luogu.record.TestCaseStatusAdapter
import org.hoshino9.luogu.record.status.RecordStatus
import org.hoshino9.luogu.record.status.RecordStatusAdapter
import org.hoshino9.luogu.record.status.RecordStatusStatusAdapter

// Utils
val globalGson : Gson by lazy {
	GsonBuilder()
			.registerTypeAdapter(TestCase.Status::class.java, TestCaseStatusAdapter)
			.registerTypeAdapter(RecordStatus.Status::class.java, RecordStatusStatusAdapter)
			.registerTypeAdapter(RecordStatus.Detail::class.java, RecordStatus.Detail)
			.registerTypeAdapter(RecordStatus::class.java, RecordStatusAdapter)
			.create()
}